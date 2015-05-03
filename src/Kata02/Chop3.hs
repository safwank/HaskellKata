module Kata02.Chop3 (chop) where

import Data.List

chop :: Int -> [Int] -> Maybe Int
chop _ [] = Nothing
chop x xs =
  let indexedXs = zip xs [0..]
  in chop' x indexedXs

chop' :: Int -> [(Int, Int)] -> Maybe Int
chop' x [(y, i)]
  | x == y    = Just i
  | otherwise = Nothing
chop' x xs
  | x <= y    = chop' x first
  | otherwise = chop' x second
  where mid = length xs `quot` 2
        (first, second) = splitAt mid xs
        (y, i) = last first
