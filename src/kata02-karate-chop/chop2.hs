import Data.List
import Utils
import Test.QuickCheck

chop :: Int -> [Int] -> Int
chop _ [] = -1
chop x xs =
  let indexedXs = zip xs [0..]
  in chop' x indexedXs

chop' :: Int -> [(Int, Int)] -> Int
chop' x [(y, i)]
  | x == y    = i
  | otherwise = -1
chop' x xs
  | x <= y    = chop' x first
  | otherwise = chop' x second
  where mid = length xs `quot` 2
        (first, second) = splitAt mid xs
        (y, i) = last first

prop_chop :: Int -> [Int] -> Bool
prop_chop x xs =
  let sortedXs = sort xs
  in chop x sortedXs == (fromJust $ elemIndex x sortedXs)
