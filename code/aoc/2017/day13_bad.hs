import Data.List.Extra

main = do
  input <- map parse . lines <$> readFile "day13.in"
  let deepest = maximum $ map fst4 input
      ansP1 = getSeverity (0, 0) input (deepest + 1)
  -- ansP2 = notCaught (0, 0) input 0 (deepest + 1)
  let updated = updateScanners $ updateScanners $ updateScanners $ updateScanners $ updateScanners $ updateScanners $ updateScanners $ updateScanners $ updateScanners $ updateScanners input
      updated' = updateScanners updated
  print updated
  print updated'
  print (getSeverity' input 0)
  print (notCaught input 0 (deepest + 1))

-- print (getSeverity (0, 0)  (deepest + 1))

-- print ansP1
-- print ansP2

-- Depth, Range, Currpos
type Scanner = (Integer, Integer, Integer, Bool)

-- depth, severity
type Packet = (Integer, Integer)

getSeverity' :: [Scanner] -> Integer -> Integer
getSeverity' [] total = total
getSeverity' (x : xs) total
  | fst4 x `mod` ((snd4 x - 1) * 2) == 0 = getSeverity' xs (total + (fst4 x * snd4 x))
  | otherwise = getSeverity' xs total

notCaught :: [Scanner] -> Integer -> Integer -> Integer
notCaught s delay deepest
  | getSeverity' s 0 == 0 = delay
  | otherwise = notCaught (updateScanners s) (delay + 1) deepest

getSeverity :: Packet -> [Scanner] -> Integer -> Integer
getSeverity p [] l = snd p
getSeverity p (x : xs) l
  | fst p == fst4 x =
      if trd4 x == 0
        then do
          getSeverity (fst p + 1, snd p + (fst4 x * snd4 x)) ys l
        else do
          getSeverity (fst p + 1, snd p) ys l
  | otherwise = do
      getSeverity (fst p + 1, snd p) (y : ys) l
  where
    (y : ys) = updateScanners (x : xs)

updateScanners :: [Scanner] -> [Scanner]
updateScanners = map updateScanner

updateScanner :: Scanner -> Scanner
updateScanner s
  -- moving downwards
  | dir && (p /= (r - 1)) = (d, r, p + 1, True)
  -- switching from downwards
  | dir && (p == (r - 1)) = (d, r, p - 1, False)
  -- moving upwards
  | not dir && (p /= 0) = (d, r, p - 1, False)
  -- switching from upwards
  | not dir && (p == 0) = (d, r, p + 1, True)
  where
    d = fst4 s
    r = snd4 s
    p = trd4 s
    dir = fth4 s

parse :: String -> Scanner
parse s =
  let spl = splitOn ": " (fst $ line1 s)
      nums = map read spl
   in (head nums, last nums, 0, True) :: Scanner

fst4 :: Scanner -> Integer
fst4 (x, _, _, _) = x

snd4 :: Scanner -> Integer
snd4 (_, x, _, _) = x

trd4 :: Scanner -> Integer
trd4 (_, _, x, _) = x

fth4 :: Scanner -> Bool
fth4 (_, _, _, x) = x
