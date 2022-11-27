import Data.List.Extra

main = do
  input <- lines <$> readFile "day6.in"
  let ansP1 = pass input
  let ansP2 = pass' input
  print ansP1
  print ansP2

pass :: [String] -> String
pass s = map mostFrequent (transpose s)

pass' :: [String] -> String
pass' s = map leastFrequent (transpose s)

mostFrequent :: String -> Char
mostFrequent ns =
  snd (maximum [(length ks, head ks) | ks <- group (sort ns)])

leastFrequent :: String -> Char
leastFrequent ns =
  snd (minimum [(length ks, head ks) | ks <- group (sort ns)])
