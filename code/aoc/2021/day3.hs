import Data.List (group, sort, transpose)

main =
  do
    file <- readFile "inputday3.txt"
    let list = map (digits . (read :: String -> Integer)) (transpose $ lines file)
        list2 = map (digits . (read :: String -> Integer)) (lines file)
        gamma = gammaRate list
        epsilon = epsilonRate gamma
        ans = binToDec gamma * binToDec epsilon

    putStrLn $ "Part 1: " ++ show ans

-- oxygenRating :: [[Integer]] -> [Integer] -> [Integer]
-- -- oxygenRating ((x : xs) : xss) (y : ys) =
-- --   filter (x y) ((x : xs) : xss)
-- oxygenRating ((x : xs) : xss) (y : ys) = map $ filter (x /= y) (xss)
-- oxygenRating _ _ = []

gammaRate :: [[Integer]] -> [Integer]
gammaRate [[]] = []
gammaRate (x : xs) = mostFrequent x : gammaRate xs
gammaRate _ = []

epsilonRate :: [Integer] -> [Integer]
epsilonRate [] = []
epsilonRate (x : xs) =
  if x == 1
    then 0 : epsilonRate xs
    else 1 : epsilonRate xs

binToDec :: [Integer] -> Integer
binToDec [] = 0
binToDec (x : xs) = x * 2 ^ length xs + binToDec xs

mostFrequent :: [Integer] -> Integer
mostFrequent ns =
  snd (maximum [(length ks, head ks) | ks <- group (sort ns)])

digits :: Integer -> [Integer]
digits 0 = []
digits input = digits (input `div` 10) ++ [input `mod` 10]
