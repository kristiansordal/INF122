module Week38Exercise1 where

mySplitAt :: Integer -> [a] -> ([a], [a])
mySplitAt n [] = ([], [])
mySplitAt n (x : xs) =
    case n of
        n > 0
            | len x <= n = x : mySplitAt xs
