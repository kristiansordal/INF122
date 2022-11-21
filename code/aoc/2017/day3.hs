main = do
  let input = 368078
      ansP1 = oddSquare input 1
  print ansP1

oddSquare :: Integer -> Integer -> Integer
oddSquare n x
  | x ^ 2 <= n = oddSquare n (x + 1)
  | x ^ 2 > n =
      if abs ((x - 1) ^ 2 - n) < abs (x ^ 2 - n)
        then x - 1 + (n - (x - 1) ^ 2)
        else x - 1 + (x ^ 2 - n)
