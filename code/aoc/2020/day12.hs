import Data.Bifunctor
import Data.List.Extra

main = do
  input <- map parse . lines <$> readFile "day12.in"
  let ansP1 = manhattanDist input ('E', 90) (0, 0)
  moveWaypoint input (0, 0) ((1, 10), 'E')

-- print ansP1

type Direction = (Char, Integer)

type Position = (Integer, Integer)

moveWaypoint :: [Direction] -> Position -> (Position, Char) -> IO Integer
moveWaypoint [] pb pw = return (abs (fst pb) + abs (snd pb))
moveWaypoint (x : xs) pb pw =
  case fst x of
    'N' -> do
      print "Boat:"
      print pb
      print "Waypoint:"
      print pw
      moveWaypoint xs pb ((fst (fst pw) + snd x, snd (fst pw)), snd pw)
    'E' -> do
      print "Boat:"
      print pb
      print "Waypoint:"
      print pw
      moveWaypoint xs pb ((fst (fst pw), snd (fst pw) + snd x), snd pw)
    'W' -> do
      print "Boat:"
      print pb
      print "Waypoint:"
      print pw
      moveWaypoint xs pb ((fst (fst pw), snd (fst pw) - snd x), snd pw)
    'S' -> do
      print "Boat:"
      print pb
      print "Waypoint:"
      print pw
      moveWaypoint xs pb ((fst (fst pw) - snd x, snd (fst pw)), snd pw)
    'R' -> do
      print "Boat:"
      print pb
      print "Waypoint:"
      print pw
      moveWaypoint xs pb (quadrant x (fst pw) (snd x))
    'L' -> do
      print "Boat:"
      print pb
      print "Waypoint:"
      print pw
      moveWaypoint xs pb (quadrant x (fst pw) (snd x))
    'F' -> do
      print "Boat:"
      print pb
      print "Waypoint:"
      print pw
      moveWaypoint xs (fst pb + snd x * fst (fst pw), snd pb + snd x * snd (fst pw)) pw

manhattanDist :: [Direction] -> Direction -> Position -> Integer
manhattanDist [] f p = abs (fst p) + abs (snd p)
manhattanDist (x : xs) f p =
  case fst x of
    'N' -> manhattanDist xs f (fst p + snd x, snd p)
    'E' -> manhattanDist xs f (fst p, snd p + snd x)
    'W' -> manhattanDist xs f (fst p, snd p - snd x)
    'S' -> manhattanDist xs f (fst p - snd x, snd p)
    'R' -> manhattanDist xs (newDist (snd f + snd x)) p
    'L' -> manhattanDist xs (newDist (snd f - snd x)) p
    'F' -> case fst f of
      'N' -> manhattanDist xs f (fst p + snd x, snd p)
      'E' -> manhattanDist xs f (fst p, snd p + snd x)
      'W' -> manhattanDist xs f (fst p, snd p - snd x)
      'S' -> manhattanDist xs f (fst p - snd x, snd p)

quadrant :: Direction -> Position -> Integer -> (Position, Char)
quadrant dir p n =
  let curr = getRotation dir
      newDir = newDist (abs curr + n)
      absPos = bimap abs abs p
   in case snd newDir of
        90 -> (second negate absPos, 'E') -- E
        180 -> (bimap negate negate absPos, 'S') -- (0 - fst absPos, 0 - snd absPos) -- S
        270 -> (first negate absPos, 'W') -- W
        360 -> (absPos, 'N')

-- N : (1,1)
-- E : (1, -1)
-- S : (-1, -1)
-- W : (-1, 1)

getRotation :: Direction -> Integer
getRotation dir =
  case find (== dir) arr of
    (Just (x, y)) -> y
    Nothing -> 0
  where
    arr = [('N', 360), ('E', 90), ('S', 180), ('W', 270)]

newDist :: Integer -> Direction
newDist f =
  (!!) arr (abs ((fromIntegral f + 360) `div` 90) `mod` 4)
  where
    arr = [('N', 360), ('E', 90), ('S', 180), ('W', 270)]

parse :: String -> Direction
parse s =
  let l = line1 s
      c = head $ fst l
      num = read $ tail $ fst l
   in (c, num)

-- 'N' -> moveWaypoint xs (fst pb + snd x * fst pw, snd pb + snd x * snd pw) pw
-- 'E' -> moveWaypoint xs (fst pb + snd x * fst pw, snd pb + snd x * snd pw) pw
-- 'W' -> moveWaypoint xs (fst pb - snd x * fst pw, snd pb - snd x * snd pw) pw
-- 'S' -> moveWaypoint xs (fst pb - snd x * snd pw, snd pb - snd x * snd pw) pw
