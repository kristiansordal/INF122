import Control.Monad
import Data.List.Extra

main = do
  input <- lines <$> readFile "day13.in"
  let earliest = read ((!!) input 0) :: Integer
      nums = map read (filter (/= "x") (splitOn "," ((!!) input 1))) :: [Integer]
      ansP1 = minWait nums earliest
  print ansP1

waitTime :: Integer -> Integer -> Integer -> (Integer, Integer)
waitTime time time' bus
  | time `mod` bus == 0 = (time - time', bus)
  | otherwise = waitTime (time + 1) time' bus

minWait :: [Integer] -> Integer -> Integer
minWait b e =
  let min = minimum $ map (waitTime e e) b
   in uncurry (*) min
