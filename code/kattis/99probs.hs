main = do
  num <- getLine
  print (getClosest99 (read num))

getClosest99 :: Integer -> Integer
getClosest99 n
  | n <= 99 = 99
  | rem == 0 = n - 1
  | rem >= 49 = n + (99 - rem)
  | rem < 49 = n - (rem - 99 + 100)
  where
    rem = n `mod` 100
