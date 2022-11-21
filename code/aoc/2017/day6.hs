import Data.Array
import Data.List.Extra
import Data.Set (Set)
import Data.Set qualified as Set

main = do
  input <- parse <$> readFile "day6.in"
  let arr = listArray (0, fromIntegral $ length input - 1) input
  print arr

parse :: String -> [Integer]
parse s =
  let n = fst $ line1 s
   in map read (splitOn " " n)
