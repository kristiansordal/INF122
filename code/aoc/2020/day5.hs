import Data.List

main = do
  file <- readFile "day5.in"
  let sorted = sort $ map (\x -> seatID x (0, 127) (0, 7)) (lines file)
  let ids = maximum sorted
  let list = mySeat sorted
  print ids
  print list

seatID :: String -> (Integer, Integer) -> (Integer, Integer) -> Integer
seatID [] (r1, r2) (c1, c2) = r2 * 8 + c2
seatID (x : xs) (r1, r2) (c1, c2)
  | x == 'B' = seatID xs (r1 + (r2 - r1) `div` 2 + 1, r2) (c1, c2)
  | x == 'F' = seatID xs (r1, r2 - (r2 - r1) `div` 2 - 1) (c1, c2)
  | x == 'R' = seatID xs (r1, r2) (c1 + (c2 - c1) `div` 2 + 1, c2)
  | x == 'L' = seatID xs (r1, r2) (c1, c2 - (c2 - c1) `div` 2 - 1)

mySeat :: [Integer] -> Integer
mySeat list
  | head list `subtract` head (tail list) == 2 = head (tail list) - 1
  | otherwise = mySeat (tail list)
