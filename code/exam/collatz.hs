main = do
  let list = map (`collatz` 0) [1000000000 .. 100000000000]
  print list

collatz :: Integer -> Integer -> Integer
collatz x c
  | even x && x /= 0 = collatz (x `div` 2) (c + 1)
  | odd x && x /= 1 = collatz (3 * x + 1) (c + 1)
  | x == 1 = c
  | otherwise = c
