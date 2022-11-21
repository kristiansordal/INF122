import Data.List.Extra

main = do
  input <- map parse . lines <$> readFile "day2.in"
  let ansP1 = diff input
      ansP2 = sum (map mapMod input)
  print ansP1
  print ansP2

diff :: [[Integer]] -> Integer
diff l = sum $ map (\x -> maximum x - minimum x) l

mapMod :: [Integer] -> Integer
mapMod l = maximum $ [x `div` y | x <- l, y <- l, y /= 0 && x /= 0 && x `mod` y == 0]

parse :: String -> [Integer]
parse s =
  let l = fst $ line1 s
   in map read (splitOn " " l)
