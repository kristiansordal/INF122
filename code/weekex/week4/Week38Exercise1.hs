module Week38Exercise1 where

mySplitAt :: Integer -> [a] -> ([a], [a])
mySplitAt n [] = ([], [])
mySplitAt n (x : xs)
  | n >= 1 = ([], [])
  | n < 1 = ([], x : xs)
