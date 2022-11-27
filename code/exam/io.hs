import Data.Char

main :: IO ()
main = do
  print "Hva heter du til fornavn?"
  name <- getLine
  let name' = map toLower name
      revName = reverse name'
      newFst = toUpper (head revName)
      revNameUpper = newFst : tail revName

  putStrLn $ "Hei, " ++ revNameUpper ++ "!"

duplicate :: [a] -> [a]
duplicate [] = []
duplicate (x : xs) = x : x : duplicate xs

f = map snd . filter (odd . fst) . zip [0 ..]

sumOfSquares :: [Integer] -> Integer
sumOfSquares list = sum ([x ^ 2 | x <- list])

sumOfSquares' :: [Integer] -> Integer
sumOfSquares' list = sum (map (^ 2) list)

hasLength :: Int -> [a] -> Bool
hasLength (-1) _ = False
hasLength 0 [] = True
hasLength n (x : xs) = hasLength (n - 1) xs

f' g x = map (g x)

f'' x y = x == y + 1
