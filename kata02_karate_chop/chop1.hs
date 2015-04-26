chop :: Int -> [Int] -> Int -- list should be Ordered
chop _ [] = -1
chop int [x]
  | int == x = 0
  | otherwise = -1
chop int list =
  let start = 0
      end = (length list) - 1
  in chop' int list start end

chop' :: Int -> [Int] -> Int -> Int -> Int
chop' int list start end
  | start == end = list !! start
  | end - start == 1 = if int == (list !! start)
                          then start
                          else if int == (list !! end)
                                  then end
                          else -1
  | otherwise =
      let mid = middle $ [start..end]
          first = list !! mid
          second = list !! (mid + 1)
      in if int <= first
        then chop' int list start mid
        else chop' int list (mid + 1) end


middle :: [Int] -> Int
middle []         = -1
middle (x:[])     = x
middle (x:y:[])   = x
middle (x:y:z:[]) = y
middle xs         = middle $ tail $ init xs
