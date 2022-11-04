import Data.List (transpose)

row :: [[Int]] -> Int -> [Int]
row m n  = m !! max 0 (n - 1)

col :: [[Int]] -> Int -> [Int]
col m n = transpose m !! max 0 (n - 1)

cols :: [[Int]] -> [[Int]]
cols = transpose

mult :: [[Int]] -> [[Int]] -> [[Int]]
mult m n = [map (multrc (row m x)) (cols n) | x <- [1 .. length m]]

multrc :: [Int] -> [Int] -> Int
multrc r c = sum [x * y | (x,y) <- zip r c]

eval :: IO()
eval =
  do
    in <- getLine
    

