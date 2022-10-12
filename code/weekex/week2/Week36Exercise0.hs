module Week36Exercise0 where

f :: Integer -> Integer -> Bool
f a b = not (even ((+) a b))

g :: Integer -> Integer -> Bool
g a b = odd $ (+) a b
