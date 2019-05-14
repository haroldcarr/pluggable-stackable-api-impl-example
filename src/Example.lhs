\iffalse

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
>
> import qualified Data.BloomFilter      as B
> import qualified Data.BloomFilter.Hash as B
> import qualified Data.Cache.LRU        as C
>
> import qualified Data.Map.Strict       as M
>
> import           Protolude             hiding (empty, fromRight, toList)
> import           Test.Hspec

\fi

\begin{center}
\textbf{\LARGE{Pluggable/Stackable\\
API Implementations}}

\bigskip

\Large{Harold Carr}

\bigskip

\normalsize{technique to configure API implementations}
\end{center}

\normalsize

------------------------------------------------------------------------------

\textbf{overview}
-----------------

- type-level programming
    - for extensible foundation
- "transformer" technique
    - to "stack" pluggins
- type-level programming
    - to configure pluggins

\url{https://icfp18.sigplan.org/details/tyde-2018/2/Authenticated-Modular-Maps-in-Haskell}

------------------------------------------------------------------------------

\textbf{example}
-----------------

\begin{picture}(0,0)(0,0)
\put(-40,-130){\includegraphics[height=3.1in]{images/cache-bloom-kv.png}}
\end{picture}

------------------------------------------------------------------------------

\textbf{motivation}
-----------------

\begin{picture}(0,0)(0,0)
\put(-40,-140){\includegraphics[height=3.1in]{images/kv-of-pages.png}}
\end{picture}

\begin{picture}(0,0)(0,0)
\put(90,-20){\includegraphics[height=0.5in]{images/crossed-out.png}}
\end{picture}

------------------------------------------------------------------------------

\textbf{example}
-----------------

\begin{picture}(0,0)(0,0)
\put(-40,-130){\includegraphics[height=3.1in]{images/cache-bloom-kv.png}}
\end{picture}

------------------------------------------------------------------------------

\textbf{hard-coded}
-----------------

~~~{.haskell}
import qualified Data.BloomFilter      as B
import qualified Data.BloomFilter.Hash as B
import qualified Data.Cache.LRU        as C
import qualified Data.Map.Strict       as M
~~~

> data MMap k v = MMap
>   { mmc :: C.LRU   k v
>   , mmb :: B.Bloom k
>   , mmm :: M.Map   k v
>   }

------------------------------------------------------------------------------

> insrt
>   :: Ord k => k -> v -> MMap k v -> MMap k v
> insrt k v m = MMap
>   (C.insert k v (mmc m))
>   (B.insert k   (mmb m))
>   (M.insert k v (mmm m))
>
> lkup :: Ord k => k -> MMap k v -> Maybe v
> lkup k m = case C.lookup k (mmc m) of
>   (_c, j@Just{}) -> j
>   (_c,  Nothing) ->
>     if B.elem k (mmb m)
>       then M.lookup k (mmm m)
>       else Nothing

------------------------------------------------------------------------------

> mmap :: MMap Int Text
> mmap  = MMap
>   (C.newLRU (Just 1024))
>   (B.empty (B.cheapHashes 3) 8192)
>   M.empty

~~~{.haskell}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
import           Protolude
import           Test.Hspec
~~~

> mmap1 :: Spec
> mmap1  = it "mmap1" $
>   lkup 1 (insrt 1 "one" mmap)
>   `shouldBe` Just "one"

------------------------------------------------------------------------------

\begin{center}

\textbf{\Large{foundations}}
\end{center}

\normalsize

------------------------------------------------------------------------------

~~~{.haskell}
import qualified Data.Map.Strict as M

:k M.Map
M.Map :: * -> * -> *

{-# LANGUAGE KindSignatures #-}

class IsMap (c :: * -> * -> *) where
~~~

------------------------------------------------------------------------------

\textbf{support constraints}

~~~{.haskell}
:t M.insert
M.insert :: Ord k
         => k -> v -> M.Map k v
         -> M.Map k v

{-# LANGUAGE TypeFamilies #-}
import GHC.Exts (Constraint)

class IsMap (c :: * -> * -> *) where

  type       MapCnstrn c k v :: Constraint

  insert :: (MapCnstrn c k v)
         => k -> v ->  c k v -> c k v
~~~

------------------------------------------------------------------------------

\textbf{support exceptions}

~~~{.haskell}
import           Control.Monad.Except

type ErM c = Except (Err c)

class IsMap (c :: * -> * -> *) where

  type       Err       c     :: *

  type       MapCnstrn c k v :: Constraint

  insert :: (MapCnstrn c k v)
         => k -> v ->  c k v -> ErM c (c k v)
~~~

------------------------------------------------------------------------------

\textbf{complete \texttt{IsMap} definition}

------------------------------------------------------------------------------

\iffalse

> type ErM c = Except (Err c)

\fi

> class IsMap (c :: * -> * -> *) where
>   type       Err       c     :: *
>   type       MapCnstrn c k v :: Constraint
>   empty  :: (MapCnstrn c k v)
>          =>                         c   k v
>   toList :: (MapCnstrn c k v)
>          =>            c k v -> ErM c [(k,v)]
>   lookup :: (MapCnstrn c k v)
>          => k ->       c k v -> ErM c (Maybe v)
>   delete :: (MapCnstrn c k v)
>          => k ->       c k v -> ErM c (c k v)
>   insert :: (MapCnstrn c k v)
>          => k -> v ->  c k v -> ErM c (c k v)
>   stats  ::            c k v -> [Text]

------------------------------------------------------------------------------

\textbf{example: wrap a normal \texttt{Data.Map}}

to show how:

- an instance is created
- the `Data.Map` `Ord` constraint is handled
- to indicate "no errors"
- to return values in the `Except` monad

------------------------------------------------------------------------------

~~~{.haskell}
import qualified Data.Map.Strict as M

data Void
~~~

> instance IsMap M.Map where
>   type       Err       M.Map     = Void
>   type       MapCnstrn M.Map k v = Ord k
>   empty        =        M.empty
>   toList       = pure . M.toList
>   lookup k   c = pure  (M.lookup k   c)
>   delete k   c = pure  (M.delete k   c)
>   insert k v c = pure  (M.insert k v c)
>   stats      c =       [show (M.size c)]

------------------------------------------------------------------------------

\textbf{a write-once map that throws errors}

to show how:

- to specify the type of errors
- to throw errors

------------------------------------------------------------------------------

> newtype WMap k v = WMap (M.Map k v)

\iffalse

>   deriving (Eq, Show)

\fi

> data WE = NP | OV

\iffalse

>   deriving (Eq, Show)

\fi

> instance IsMap WMap where
>   type       Err       WMap      = WE
>   type       MapCnstrn WMap  k v = Ord k

\iffalse

>   empty               = WMap M.empty
>   toList     (WMap m) = pure (M.toList m)

\fi

>   lookup k   (WMap m) = case M.lookup k m of
>     Nothing -> throwError NP
>     mv      -> pure mv

\iffalse

>   delete k   (WMap m) = case M.lookup k m of
>     Nothing -> throwError NP
>     _       -> pure (WMap (M.delete k m))

\fi

>   insert k v (WMap m) = case M.lookup k m of
>     Nothing -> pure (WMap (M.insert k v m))
>     _       -> throwError OV

\iffalse

>   stats      (WMap m) = [show (M.size m)]

\fi

------------------------------------------------------------------------------

\iffalse

> wm1 :: Spec
> wm2 :: Spec
> wm3 :: Spec
> emptyWMap,x :: WMap Int Int

\fi

> emptyWMap = empty :: WMap Int Int

> wm1  = it "wm1" $
>   runExcept (toList emptyWMap)
>   `shouldBe` Right []
>

> wm2  = it "wm2" $
>   runExcept (lookup 2 emptyWMap)
>   `shouldBe` Left NP
>

> x = fromRight
>      (runExcept (insert 2 2 emptyWMap))
> wm3  = do
>   it "wm3-stats" $ stats x `shouldBe` ["1"]
>   it "wm3-ov"    $ runExcept (insert 2 2 x)
>                    `shouldBe` Left OV

------------------------------------------------------------------------------

\textbf{directly combining a Map with a Cache}

------------------------------------------------------------------------------

\small

> data CMap k v = CMap (C.LRU k v) (M.Map k v)
>
> instance IsMap CMap where
>   type Err       CMap     = Void
>   type MapCnstrn CMap k v = Ord k
>   empty = CMap (C.newLRU (Just 1000)) M.empty -- TODO hardcoded config
>   toList     (CMap _ m) = pure (M.toList m)
>   lookup k   (CMap h m) = case C.lookup k h of
>     (_, j@(Just _)) -> pure j -- TODO : updated LRU
>     _               -> pure (M.lookup k m)

\iffalse

>   delete k   (CMap h m) =
>     pure (CMap (fst (C.delete k   h))
>                     (M.delete k   m))

\fi

>   insert k v (CMap h m) =
>     pure (CMap      (C.insert k v h)
>                     (M.insert k v m))
>   stats      (CMap h m)
>     = show (C.size h) <> "/" <> show (C.maxSize h)
>     : stats m

\normalsize

------------------------------------------------------------------------------

\iffalse

> cm1 :: Spec

\fi

> cm1 = it "cm1" $
>   stats (empty :: CMap Int Int)
>   `shouldBe ` ["0/Just 1000","0"]

\iffalse

> cm2 :: Spec

\fi

> cm2 = it "cm2" $
>   stats
>    (fromRight
>     (runExcept
>      (insert 2 20 (empty :: CMap Int Int))))
>   `shouldBe` ["1/Just 1000","1"]

------------------------------------------------------------------------------

\textbf{directly combining a Map with a Bloom filter}

------------------------------------------------------------------------------

\small

> data Blm1 k v = Blm1 (B.Bloom k) (M.Map k v)
>
> instance IsMap   Blm1 where
>   type Err       Blm1      = Err M.Map
>   type MapCnstrn Blm1  k v =
>      ( MapCnstrn M.Map k v
>      , B.Hashable      k )
>   empty = Blm1 (B.empty (B.cheapHashes 3) 8192) -- TODO hardcoded config
>                M.empty
>   toList     (Blm1 _ m) = pure (M.toList m)
>   lookup k   (Blm1 b m)
>     | B.elem k b = pure (M.lookup k m)
>     | otherwise  = pure Nothing

\iffalse

>   delete = undef

\fi

>   insert k v (Blm1 b m) =
>     pure (Blm1 (B.insert k   b)
>                (M.insert k v m))

\iffalse

>   stats = undef

\fi

\normalsize

------------------------------------------------------------------------------

\textbf{higher-order instances via "transformers" technique}

- above, when combining
    - cache with map
    - bloom with map
- NO intrinsic `M.Map` info is used

so abstract `M.Map` via a type variable to get higher-order instances

------------------------------------------------------------------------------

\small

~~~{.haskell}
:k Monad
  ::  (* -> *)      -> Constraint

:k IsMap
  ::  (* -> * -> *) -> Constraint

:k MonadTrans
  :: ((* -> *)      -> * -> *)      -> Constraint

:k IsMapTrans
  :: ((* -> * -> *) -> * -> * -> *) -> Constraint

class MonadTrans t where
  lift :: (Monad m) => m a   -> t m a

class IsMapTrans t where
  lift :: (IsMap c) => c k v -> t c k v
~~~

\normalsize

------------------------------------------------------------------------------

\begin{center}
\textbf{stackable pluggins}

\bigskip

\textbf{i.e., higher-order instances}
\end{center}

------------------------------------------------------------------------------

\small

> data Blm2 c k v = Blm2 (B.Bloom k) (c k v)
>
> instance IsMap c => IsMap (Blm2 c) where
>   type Err       (Blm2 c)     = Err c
>   type MapCnstrn (Blm2 c) k v =
>      ( MapCnstrn c        k v
>      , B.Hashable         k )
>   empty = Blm2 (B.empty (B.cheapHashes 3) 8192) -- TODO hardcoded
>                empty
>   toList     (Blm2 _ c) = toList c
>   lookup k   (Blm2 b c)
>     | B.elem k b = lookup k c
>     | otherwise  = pure Nothing

\iffalse

>   delete = undef

\fi

>   insert k v (Blm2 b c)
>     =  Blm2 (B.insert k   b)
>    <$>         insert k v c -- note
>   stats      (Blm2 b c) = show (B.length b) : stats c

\normalsize

------------------------------------------------------------------------------

\textbf{configuring the bloom filter at the type level}

~~~{.haskell}
{-# LANGUAGE DataKinds, PolyKinds #-}
import GHC.TypeLits
~~~

------------------------------------------------------------------------------

\small

> newtype SBlm nHash nBits k = SBlm (B.Bloom k)
>
> data BlmOf (nHash :: Nat) (nBits :: Nat)
>            (c :: * -> * -> *) k v
>    = BlmOf (SBlm nHash nBits k) (c k v)
>
> instance (KnownNat nHash, KnownNat nBits, IsMap c)
>   => IsMap       (BlmOf nHash nBits c) where
>   type Err       (BlmOf nHash nBits c)     = Err c
>   type MapCnstrn (BlmOf nHash nBits c) k v =
>      ( MapCnstrn c k v, B.Hashable k
>      , KnownNat nHash, KnownNat nBits )
>   empty = BlmOf (bEmpty Proxy Proxy) empty

\iffalse

>   toList     (BlmOf _        c) = toList c
>   lookup k   (BlmOf (SBlm b) c)
>     | B.elem k b = lookup k c
>     | otherwise  = pure Nothing
>   delete = undef
>   insert k v (BlmOf (SBlm b) c)
>     =  BlmOf (SBlm (B.insert k   b))
>    <$>                insert k v c
>   stats      (BlmOf (SBlm b) c)
>     = show (B.length b)
>     : stats c

\fi

> bEmpty :: (KnownNat h, KnownNat b, B.Hashable k)
>        => Proxy h -> Proxy b -> SBlm h b k
> bEmpty h b = SBlm
>   (B.empty (B.cheapHashes (fromIntegral $ natVal h))
>            (fromIntegral $ natVal b))

\normalsize

------------------------------------------------------------------------------

\iffalse

> blm1 :: Spec

\fi

> blm1 = it "blm1" $
>   runExcept
>    (toList
>     (empty :: BlmOf 3 4096 M.Map Int Int))
>   `shouldBe` Right []

\iffalse

> blm2 :: Spec

\fi

> blm2 = it "blm2" $
>   stats (empty :: BlmOf 3 4096 M.Map Int Int)
>   `shouldBe` ["4096","0"]

------------------------------------------------------------------------------

\small

\textbf{cache plugin}

> newtype SCache bound k v = SCache (C.LRU k v)
>
> data CacheOf (bound :: Nat) (c :: * -> * -> *) k v
>    = CacheOf (SCache bound k v) (c k v)
>
> instance (KnownNat bound, IsMap c)
>   => IsMap       (CacheOf bound c) where
>   type Err       (CacheOf bound c)     = Err c
>   type MapCnstrn (CacheOf bound c) k v =
>      ( MapCnstrn c k v, Ord k, KnownNat bound )
>   empty = CacheOf (cEmpty Proxy) empty

\iffalse

>   toList     (CacheOf _          c) = toList c
>   lookup k   (CacheOf (SCache h) c) =
>     case C.lookup k h of
>       (_, j@(Just _)) -> pure j
>       _               -> lookup k c
>   delete = undef
>   insert k v (CacheOf (SCache h) c)
>     =  CacheOf (SCache (C.insert k v h))
>    <$>                    insert k v c
>   stats      (CacheOf (SCache h) c)
>     = show (C.size h) <> "/" <> show (C.maxSize h)
>     : stats c

\fi

> cEmpty :: (Ord k, KnownNat bound)
>        => Proxy bound -> SCache bound k v
> cEmpty bound = SCache
>   (C.newLRU (Just (fromIntegral $ natVal bound)))

\normalsize

------------------------------------------------------------------------------

\textbf{specifying a cache/bloom/map}

> type CB bound nHash nBits (c :: * -> * -> *)
>   = CacheOf bound
>       (BlmOf nHash nBits
>         c)

> cb :: CB 20 3 4096 M.Map Int Int
> cb = fromRight (runExcept (insert 1 1 empty))

\iffalse

> cb1 :: Spec

\fi

> cb1 = it "cb1" $
>   fromRight (runExcept (toList cb))
>   `shouldBe` [(1,1)]

\iffalse

> cb2 :: Spec

\fi

> cb2 = it "cb2" $ stats (fromRight (runExcept
>   (insert 1 1
>     (empty :: CB 20 3 4096 M.Map Int Int))))
>   `shouldBe` ["1/Just 20","4096","1"]

------------------------------------------------------------------------------

\small

~~~{.haskell}
stack test

PS
  pluggable-stackable
    wm1
    wm2
    wm3-stats
    wm3-ov
    cm1
    cm2
    blm1
    blm2
    cb1
    cb2

Finished in 0.0007 seconds
10 examples, 0 failures
~~~

\normalsize

------------------------------------------------------------------------------

\url{https://github.com/haroldcarr/pluggable-stackable-api-impl-example}

\bigskip
\bigskip
\bigskip
\bigskip

\url{https://www.researchgate.net/publication/327738906_Authenticated_modular_maps_in_Haskell}

\iffalse

> undef :: a
> undef  = panic "undefined"

> fromRight :: Either e a -> a
> fromRight x0 = case x0 of
>   Left _ -> undef
>   Right r -> r

\fi
