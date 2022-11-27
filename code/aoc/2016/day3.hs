import Combinatorics
import Data.List.Extra

main = do
  input <- map parse . lines <$> readFile "day3.in"
  let ansP1 = validTriangles (map validCombs (transposeList input []))
  print ansP1

transposeList :: [[Integer]] -> [[Integer]] -> [[Integer]]
transposeList [] l = l
transposeList (x : y : z : zs) l = transposeList zs (l ++ transpose [x, y, z])

validTriangles :: [Bool] -> Int
validTriangles = length . filter (== True)

validCombs :: [Integer] -> Bool
validCombs l =
  let perms = permute l
   in all valid perms

valid :: [Integer] -> Bool
valid l = ((!!) l 0 + (!!) l 1) > (!!) l 2

parse :: String -> [Integer]
parse s =
  let l = line1 s
      split = map read (splitOn " " (fst l))
   in split
