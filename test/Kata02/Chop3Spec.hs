module Kata02.Chop3Spec (spec) where

import Data.List
import Test.Hspec
import Test.Hspec.QuickCheck
import Kata02.Chop3

spec :: Spec
spec = do
  describe "#chop" $ do
    prop "equals elemIndex" $
      \x xs -> let sortedXs = sort xs
               in chop x sortedXs == elemIndex x sortedXs