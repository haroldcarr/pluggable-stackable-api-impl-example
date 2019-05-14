> {-# OPTIONS_GHC -fprint-explicit-kinds #-}

> {-# LANGUAGE DataKinds            #-}
> {-# LANGUAGE KindSignatures       #-}
> {-# LANGUAGE OverloadedStrings    #-}
> {-# LANGUAGE PolyKinds            #-}
> {-# LANGUAGE NoImplicitPrelude    #-}
> {-# LANGUAGE TypeFamilies         #-}
> {-# LANGUAGE UndecidableInstances #-}
>
> module Example where
>
> import           Control.Monad.Except
> import qualified Data.BloomFilter      as B
> import qualified Data.BloomFilter.Hash as B
> import qualified Data.Cache.LRU        as C
> import qualified Data.Map.Strict       as M
> import           GHC.Exts              (Constraint)
> import           Protolude             hiding (empty, toList)
> import           Test.HUnit            (Counts, Test (TestList), runTestTT)
> import qualified Test.HUnit.Util       as U (t)

import qualified Data.Map.Strict as M

:t M.insert
M.insert :: Ord k => k -> v -> M.Map k v -> M.Map k v

:k M.Map
M.Map :: * -> * -> *

> type ErrM c = Except (Err c)
>
> class IsMap (c :: * -> * -> *) where
>   type       Err        c     :: *
>   type       IsMapCnstr c k v :: Constraint
>   empty  :: (IsMapCnstr c k v)
>          =>             c k v
>   toList :: (IsMapCnstr c k v)
>          =>             c k v -> ErrM c [(k,v)]
>   lookup :: (IsMapCnstr c k v)
>          => k ->        c k v -> ErrM c (Maybe v)
>   delete :: (IsMapCnstr c k v)
>          => k ->        c k v -> ErrM c (c k v)
>   insert :: (IsMapCnstr c k v)
>          => k -> v ->   c k v -> ErrM c (c k v)

data Void

> instance IsMap M.Map where
>   type       Err        M.Map     = Void
>   type       IsMapCnstr M.Map k v = Ord k
>   empty        =          M.empty
>   toList       = return . M.toList
>   lookup k   c = return  (M.lookup k   c)
>   delete k   c = return  (M.delete k   c)
>   insert k v c = return  (M.insert k v c)

> newtype WMap k v = WMap (M.Map k v)
> data WE = NP | OV deriving (Eq, Show)
>
> instance IsMap WMap where
>   type       Err        WMap      = WE
>   type       IsMapCnstr WMap  k v = Ord k
>   empty               = WMap M.empty
>   toList     (WMap m) = return (M.toList m)
>   lookup k   (WMap m) = case M.lookup k m of
>     Nothing -> throwError NP
>     mv      -> return mv
>   delete k   (WMap m) = case M.lookup k m of
>     Nothing -> throwError NP
>     _       -> return (WMap (M.delete k m))
>   insert k v (WMap m) = case M.lookup k m of
>     Nothing -> return (WMap (M.insert k v m))
>     _       -> throwError OV

> wm1 :: [Test]
> wm1  = U.t "wm1"
>   (runExcept $ toList (empty :: WMap Int Int))
>   (Right [])


> data CMap k v = CMap (C.LRU k v) (M.Map k v)
>
> instance IsMap CMap where
>   type Err        CMap     = Void
>   type IsMapCnstr CMap k v = Ord k
>   empty = CMap (C.newLRU Nothing) M.empty
>   toList     (CMap _ m) = return (M.toList m)
>   -- TODO : no return of updated LRU
>   lookup k   (CMap c m) = case C.lookup k c of
>     (_, j@(Just _)) -> return j
>     _               ->
>       case M.lookup k m of
>         Nothing -> return Nothing
>         j       -> return j
>   delete k   (CMap c m) =
>     return (CMap (fst (C.delete k   c))
>                       (M.delete k   m))
>   insert k v (CMap c m) =
>     return (CMap      (C.insert k v c)
>                       (M.insert k v m))

------------------------------------------------------------------------------

key-value store transformer, t has kind : (* -> * -> *) -> * -> * -> *
then
instance IsMap c ⇒ IsMap (t c) where ...

base map

------------------------------------------------------------------------------

add-ons

directly combining a Map with a Bloom filter

> data Blm1 k v = Blm1 (B.Bloom k) (M.Map k v)
>
> instance IsMap Blm1 where
>   type Err Blm1 = Err M.Map
>   type IsMapCnstr Blm1  k v =
>      ( IsMapCnstr M.Map k v
>      , B.Hashable       k )
>   empty = Blm1 (B.empty (B.cheapHashes 3) 10000) -- TODO hardcoded
>                M.empty
>   toList     (Blm1 _ m) = return (M.toList m)
>   lookup k   (Blm1 b m)
>     | B.elem k b = return (M.lookup k m)
>     | otherwise  = return Nothing
>   delete = undef
>   insert k v (Blm1 b m) =
>     return (Blm1 (B.insert k   b)
>                  (M.insert k v m))

nothing above using intrinsic info about M.Map

abstract M.Map via a type variable to get a higher-order version of BloomOf :

> data Blm2 c k v = Blm2 (B.Bloom k) (c k v)
>
> instance IsMap c => IsMap (Blm2 c) where
>   type Err (Blm2 c) = Err c
>   type IsMapCnstr (Blm2 c) k v =
>      ( IsMapCnstr c        k v
>      , B.Hashable          k )
>   empty = Blm2 (B.empty (B.cheapHashes 3) 10000) -- TODO hardcoded
>                empty
>   toList   (Blm2 _ c) = toList c
>   lookup k (Blm2 b c)
>     | B.elem k b = lookup k c
>     | otherwise  = return Nothing
>   delete = undef
>   insert k v (Blm2 b c)
>     =  Blm2 (B.insert k   b)
>    <$>         insert k v c

configuring the bloom filter

> newtype SBlm nHash bits k = SBlm (B.Bloom k)
>
> data BlmOf (nHash :: Nat) (nBits :: Nat) (c :: * -> * -> *) k v
>    = BlmOf (SBlm nHash nBits k) (c k v)
>
> instance (KnownNat nHash, KnownNat nBits, IsMap c)
>   => IsMap (BlmOf nHash nBits c) where
>   type Err (BlmOf nHash nBits c) = Err c
>   type IsMapCnstr (BlmOf nHash nBits c) k v =
>      ( IsMapCnstr c        k v
>      , B.Hashable          k
>      , KnownNat nHash, KnownNat nBits )
>   empty = BlmOf (bEmpty Proxy Proxy) empty
>   toList     (BlmOf _        c) = toList c
>   lookup k   (BlmOf (SBlm b) c)
>     | B.elem k b = lookup k c
>     | otherwise  = return Nothing
>   delete = undef
>   insert k v (BlmOf (SBlm b) c)
>     =  BlmOf (SBlm (B.insert k   b))
>    <$>                insert k v c

> bEmpty :: (KnownNat h, KnownNat b, B.Hashable k)
>        => Proxy h -> Proxy b -> SBlm h b k
> bEmpty h b = SBlm (B.empty (B.cheapHashes (fromIntegral $ natVal h))
>                            (fromIntegral $ natVal b))

> blm1 :: [Test]
> blm1  = U.t "blm1"
>   (runExcept $ toList (empty :: BlmOf 3 10000 M.Map Int Int))
>   (Right [])

------------------------------------------------------------------------------

> undef :: a
> undef  = panic "undefined"

------------------------------------------------------------------------------

> t :: IO Counts
> t = runTestTT
>   $ TestList $ wm1 ++ blm1
