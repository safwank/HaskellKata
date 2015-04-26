import Data.List
import Utils
import Test.QuickCheck

chop :: Int -> [Int] -> Int
chop _ [] = -1
chop x [y]
  | x == y = 0
  | otherwise = -1
chop x xs =
  let zipped_xs = zip xs [0..]
  in chop' x zipped_xs

chop' :: Int -> [(Int, Int)] -> Int
chop' x [(y, i)]
  | x == y = i
  | otherwise = -1
chop' x xs =
  let mid = length xs `quot` 2
      (first, second) = splitAt mid xs
      (y, i) = last first in
  if x <= y
     then chop' x first
     else chop' x second

prop_chop :: Int -> [Int] -> Bool
prop_chop x xs =
  let sorted_xs = sort xs
  in chop x sorted_xs == (fromJust $ elemIndex x sorted_xs)
