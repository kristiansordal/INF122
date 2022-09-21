main =
  do
    file <- readFile "inputday1.txt"
    let ls = lines file
        numbers = map read ls :: [Integer]
        numbersThree = triplets numbers
        count = countIncrement numbers
        countThree = countIncrement numbersThree
        countThreeV2 = countIncrementsThree numbers
    putStrLn $ "Part 1: " ++ show count
    putStrLn $ "Part 2: " ++ show countThree
    putStrLn $ "Part 2 V2: " ++ show countThreeV2

countIncrement :: [Integer] -> Integer
countIncrement (x : y : ys) =
  if x < y
    then 1 + countIncrement (y : ys)
    else countIncrement (y : ys)
countIncrement _ = 0

triplets :: [Integer] -> [Integer]
triplets (x : y : z : zs) = x + y + z : triplets (y : z : zs)
triplets _ = []

countIncrementsThree :: [Integer] -> Integer
countIncrementsThree (w : x : y : z : zs) =
  if w + x + y < x + y + z
    then 1 + countIncrementsThree (x : y : z : zs)
    else countIncrementsThree (x : y : z : zs)
countIncrementsThree _ = 0
