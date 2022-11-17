import Data.List

main = do
  nums <- readFile "day10.in" >>= return . parse
  let nums' = sort nums
      ansP1 = joltsDiff nums' 0 0 1
  print ansP1

joltsDiff :: [Integer] -> Integer -> Integer -> Integer -> Integer
joltsDiff [] p n m = n * m
joltsDiff (x : xs) p n m
  | (x - p) == 1 = joltsDiff xs x (n + 1) m
  | (x - p) == 2 = joltsDiff xs x n m
  | (x - p) == 3 = joltsDiff xs x n (m + 1)

parse :: String -> [Integer]
parse s = map read (lines s)
