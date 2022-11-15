import Data.List
import Data.List.Split

main = do
  nums <- getLine
  chars <- getLine
  let nums' = sort (map read (splitOn " " nums) :: [Int])
      ans = matchIndex nums' chars
  putStrLn . unwords . map show $ ans

matchIndex :: [Int] -> String -> [Int]
matchIndex _ [] = []
matchIndex list (y : ys)
  | y == 'A' = head list : matchIndex list ys
  | y == 'B' = head (tail list) : matchIndex list ys
  | y == 'C' = last list : matchIndex list ys
