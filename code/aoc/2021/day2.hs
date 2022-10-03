main = do
  file <- readFile "inputday2.txt"
  let ls = lines file
  let commands = map splitString ls
  let ans = finalPos commands 0 0
  let ans2 = finalPos2 commands 0 0 0
  putStrLn $ "Part 1: " ++ show ans
  putStrLn $ "Part 2: " ++ show ans2

splitString :: String -> (String, Integer)
splitString string = (a, b)
  where
    list = words string
    a = head list
    b = read $ last list

finalPos :: [(String, Integer)] -> Integer -> Integer -> Integer
finalPos [] h d = h * d
finalPos (x : xs) h d
  | fst x == "forward" = finalPos xs (h + snd x) d
  | fst x == "down" = finalPos xs h (d + snd x)
  | fst x == "up" = finalPos xs h (d - snd x)
finalPos _ _ _ = 0

finalPos2 :: [(String, Integer)] -> Integer -> Integer -> Integer -> Integer
finalPos2 [] h d a = h * d
finalPos2 (x : xs) h d a
  | fst x == "forward" = finalPos2 xs (h + snd x) (d + (snd x * a)) a
  | fst x == "down" = finalPos2 xs h d (a + snd x)
  | fst x == "up" = finalPos2 xs h d (a - snd x)
finalPos2 _ _ _ _ = 0
