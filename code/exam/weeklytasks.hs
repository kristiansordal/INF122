-- Week 35
f :: Integer -> Integer
f n = n * (n + 1) `div` 2

triangle :: Integer -> Integer
triangle x = sum [1 .. x]

repeat2 :: [a] -> [a]
repeat2 x = x ++ x

-- Week 36
f' :: Integer -> Integer -> Bool
f' x y = odd ((+) x y)

f'' :: [Integer] -> Integer -> Bool
f'' xs x = (>) x (sum xs)

halfPalindrome :: String -> Maybe String
halfPalindrome s
  | s == reverse s = Just (take (length s `div` 2) s)
  | otherwise = Nothing

decomposePalindrome :: String -> Maybe (String, Maybe Char)
decomposePalindrome s
  | s == reverse s =
      case () of
        ()
          | even $ length s -> Just (take (length s `div` 2) s, Nothing)
          | otherwise -> Just (take (length s `div` 2) s, Just (s !! (length s `div` 2)))
  | otherwise = Nothing

createPalindrome :: String -> Maybe Char -> String
createPalindrome s (Just c) = s ++ [c] ++ reverse s
createPalindrome s Nothing = s ++ reverse s

-- Week 37
namesAndAges :: [String] -> [Integer] -> [String]
namesAndAges names ages = map
  where
    showAges = map (show ages)
    list = zip names showAges
