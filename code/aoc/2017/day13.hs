import Data.List.Extra

main = do
  input <- map parse . lines <$> readFile "day13.in"
  let ansP1 = getSeverity input 0
  print ansP1

type Scanner = (Integer, Integer)

getSeverity :: [Scanner] -> Integer -> Integer
getSeverity [] total = total
getSeverity (x : xs) total
  | fst x `mod` ((snd x - 1) * 2) == 0 = getSeverity xs (total + uncurry (*) x)
  | otherwise = getSeverity xs total

notCaught :: [Scanner] -> Integer -> Integer
notCaught p s delay
  | getSeverity p s == 0 = delay
  | otherwise = notCaught p s (delay + 1)

parse :: String -> Scanner
parse s =
  let spl = splitOn ": " (fst $ line1 s)
      nums = map read spl
   in (head nums, last nums) :: Scanner
