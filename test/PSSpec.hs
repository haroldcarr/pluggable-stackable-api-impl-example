module PSSpec where

import           Example
import           Test.Hspec

spec :: Spec
spec = describe "pluggable-stackable" $ do
  wm1
  wm2
  wm3
  cm1
  cm2
  blm1
  blm2
  cb1
  cb2

