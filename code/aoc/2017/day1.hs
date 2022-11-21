import Data.Char
import Data.List.Extra

main = do
  input <- parse . fst . line1 <$> readFile "day1.in"
  let input' = input ++ [head input]
      ansP1 = getSum input'
      ansP2 = getSum' (input ++ input) (fromIntegral $ length input `div` 2) (fromIntegral $ length input)
  print ansP1
  print ansP2

getSum :: [Integer] -> Integer
getSum (x : y : ys)
  | x == y = x + getSum (y : ys)
  | otherwise = getSum (y : ys)
getSum _ = 0

getSum' :: [Integer] -> Integer -> Integer -> Integer
getSum' l i x
  | length l == fromIntegral x = 0
  | head l == (!!) l (fromIntegral i) = head l + getSum' (tail l) i x
  | otherwise = getSum' (tail l) i x

parse :: String -> [Integer]
parse = map (\x -> read [x])
