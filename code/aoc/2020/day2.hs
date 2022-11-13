import Data.List.Split

main = do
  file <- readFile "day2.in"
  let line = lines file
  let nums = map ((\x -> (read $ head x, read $ last x)) . splitOn "-" . head . words) line :: [(Integer, Integer)]
  let cs = map (head . (!! 1) . words) line
  let password = map (last . words) line
  let valid = isValid nums cs password 0
  let valid' = isValidIndex nums cs password 0
  print valid
  print valid'

isValid :: [(Integer, Integer)] -> String -> [String] -> Integer -> Integer
isValid [] [] [] n = n
isValid (x : xs) s (y : ys) n
  | letters y (head s) x = isValid xs (tail s) ys (n + 1)
  | otherwise = isValid xs (tail s) ys n

isValidIndex :: [(Integer, Integer)] -> String -> [String] -> Integer -> Integer
isValidIndex [] [] [] n = n
isValidIndex (x : xs) s (y : ys) n
  | indexed y (head s) x = isValidIndex xs (tail s) ys (n + 1)
  | otherwise = isValidIndex xs (tail s) ys n

indexed :: String -> Char -> (Integer, Integer) -> Bool
indexed s c (x, y) =
  (!! (fromInteger x - 1)) s == c && (!! (fromInteger y - 1)) s /= c
    || (!! (fromInteger x - 1)) s /= c && (!! (fromInteger y - 1)) s == c

letters :: String -> Char -> (Integer, Integer) -> Bool
letters s c (x, y) = len >= fromIntegral x && len <= fromIntegral y
  where
    len = length $ filter (== c) s
