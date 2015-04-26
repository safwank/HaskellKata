import Data.List
import Test.QuickCheck

chop :: Int -> [Int] -> Int -- list should be Ordered
chop _ [] = -1
chop x [y]
  | x == y = 0
  | otherwise = -1
chop x xs =
  let start = 0
      end = (length xs) - 1
  in chop' x xs start end

chop' :: Int -> [Int] -> Int -> Int -> Int
chop' x xs start end
  | start == end = if x == (xs !! start)
                      then start
                      else -1
  | otherwise =
      let mid = middle $ [start..end]
          first = xs !! mid
          second = xs !! (mid + 1) in
      if x <= first
         then chop' x xs start mid
         else chop' x xs (mid + 1) end

middle :: [Int] -> Int
middle []       = -1
middle (x:[])   = x
middle (x:y:[]) = x
middle xs       = middle $ tail $ init xs

fromJust :: Maybe Int -> Int
fromJust Nothing = -1
fromJust (Just x) = x

prop_chop :: Int -> [Int] -> Bool
prop_chop x xs =
  let sorted_xs = sort xs
  in chop x sorted_xs == (fromJust $ elemIndex x sorted_xs)
