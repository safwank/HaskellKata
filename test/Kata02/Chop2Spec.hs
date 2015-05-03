module Kata02.Chop2Spec (spec) where

import Data.List
import Test.Hspec
import Test.Hspec.QuickCheck
import Kata02.Chop2
import Kata02.Utils

spec :: Spec
spec = do
  describe "#chop" $ do
    prop "equals elemIndex" $
      \x xs -> let sortedXs = sort xs
               in chop x sortedXs == (fromJust $ elemIndex x sortedXs)

