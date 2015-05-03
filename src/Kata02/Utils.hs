module Kata02.Utils (middle, fromJust) where

middle :: [Int] -> Int
middle []       = -1
middle (x:[])   = x
middle (x:y:[]) = x
middle xs       = middle $ tail $ init xs

fromJust :: Maybe Int -> Int
fromJust Nothing = -1
fromJust (Just x) = x
