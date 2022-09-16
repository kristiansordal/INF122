module Week38Exercise1 where

mySplitAt :: Integer -> [a] -> ([a], [a])
mySplitAt n [] = ([], [])
mySplitAt n (x : xs)
  | n <= 0 = ([], x : xs)
  | otherwise = (x : firstHalf, secondHalf)
  where
    (firstHalf, secondHalf) = mySplitAt (n - 1) xs
