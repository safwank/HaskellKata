module Kata02.Chop4 (chop) where

import qualified Data.List as L
import qualified Data.Vector as V

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