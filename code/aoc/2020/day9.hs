main = do
  nums <- readFile "day9.in" >>= return . parse

-- isSum :: [Integer] -> (Integer, Integer) -> Integer
-- isSum (x:xs) (l, r) =
-- let window = take r drop l (x:xs)

twoSum :: [Integer] -> Integer -> Integer -> Bool
twoSum (x : xs) c t
  | (x + c) == t = True
  | otherwise = twoSum xs x t

parse :: String -> [Integer]
parse s = map read (lines s)
