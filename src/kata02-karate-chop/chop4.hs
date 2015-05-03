import qualified Data.List as L
import qualified Data.Vector as V
import Test.QuickCheck

chop :: Int -> V.Vector Int -> Maybe Int
chop x xs = chop' x xs 0 (V.length xs - 1)

chop' :: Int -> V.Vector Int -> Int -> Int -> Maybe Int
chop' x xs low high
  | low > high = Nothing
  | x < midVal = chop' x xs low (midIdx - 1)
  | x > midVal = chop' x xs (midIdx + 1) high
  | otherwise  = Just midIdx
  where midIdx = low + (high - low) `div` 2
        midVal = xs V.! midIdx

prop_chop :: Int -> [Int] -> Bool
prop_chop x xs =
  let uniqueSortedXs = L.nub $ L.sort xs
  in chop x (V.fromList uniqueSortedXs) == L.elemIndex x uniqueSortedXs