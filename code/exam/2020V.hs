import Data.List (transpose)
import Text.Read hiding (Number)

row :: [[Int]] -> Int -> [Int]
row m n = m !! max 0 (n - 1)

col :: [[Int]] -> Int -> [Int]
col m n = transpose m !! max 0 (n - 1)

cols :: [[Int]] -> [[Int]]
cols = transpose

mult :: [[Int]] -> [[Int]] -> [[Int]]
mult m n = [map (multrc (row m x)) (cols n) | x <- [1 .. length m]]

multrc :: [Int] -> [Int] -> Int
multrc r c = sum [x * y | (x, y) <- zip r c]

data Input
  = Operation (Int -> Int -> Int)
  | Number Int

parseInput "+" = Just $ Operation (+)
parseInput "-" = Just $ Operation subtract
parseInput "*" = Just $ Operation (*)
parseInput x = Number <$> readMaybe x

eval :: IO ()
eval = process []

process :: [Int] -> IO ()
process stack =
  do
    print stack
    cmd <- parseInput <$> getLine
    case cmd of
      Nothing -> do
        putStrLn "Invalid input"
        process stack
      Just (Operation f) -> do
        case stack of
          (x : y : rest) -> process (f y x : rest)
          _ -> do
            putStrLn "Operation lacks arguments"
            process stack
      Just (Number n) -> process (n : stack)
