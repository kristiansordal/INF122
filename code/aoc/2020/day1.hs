main =
  do
    file <- readFile "inputday1.txt"
    let ls = lines file
        numbers = map read ls :: [Integer]
        oneNum = twoSum numbers
    putStrLn $ "Part 1: " ++ show oneNum

twoSum :: [Integer] -> Integer
twoSum (x : xs) =
  if sumAll x (x : xs) + x == 2020
    then x * sumAll x (x : xs)
    else twoSum xs
twoSum _ = 0

sumAll :: Integer -> [Integer] -> Integer
sumAll n (x : xs) =
  if n + x == 2020
    then x
    else sumAll n xs
sumAll n _ = 0
