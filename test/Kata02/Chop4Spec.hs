module Kata02.Chop4Spec (spec) where

import qualified Data.List as L
import qualified Data.Vector as V
import Test.Hspec
import Test.Hspec.QuickCheck
import Kata02.Chop4
import Kata02.Utils

spec :: Spec
spec = do
  describe "#chop" $ do
    prop "equals elemIndex" $
      \x xs -> let uniqueSortedXs = L.nub $ L.sort xs
               in chop x (V.fromList uniqueSortedXs) == L.elemIndex x uniqueSortedXs