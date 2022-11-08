main = do
  file <- readFile "day1.in"
  let list = lines file
      nums = map read list
      ansP1 = fuelSum nums
      ansP2 = fuelSum' nums

  putStrLn $ "Part 1: " ++ show ansP1
  putStrLn $ "Part 2: " ++ show ansP2

fuel :: Integer -> Integer
fuel x
  | (x `div` 3) - 2 <= 0 = 0
  | otherwise = (x `div` 3) - 2

fuelSum :: [Integer] -> Integer
fuelSum list = sum (map fuel list)

fuelSum' :: [Integer] -> Integer
fuelSum' list
  | not (all (== 0) needed) = sum needed + fuelSum' needed
  | otherwise = sum needed
  where
    needed = map fuel list
