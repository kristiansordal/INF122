import Data.List.Extra
import Data.Set (Set)
import Data.Set qualified as Set

main = do
  input <- parse <$> readFile "day1.in"
  let pos = dist input 'N' 0 0 []
      ansP1 = abs (fst $ head pos) + abs (snd $ head pos)
      ansP2 = posSet pos Set.empty
  print ansP1
  print ansP2

type Direction = (Char, Integer)

type Cartesian = Char

type Position = (Integer, Integer)

posSet :: [Position] -> Set Position -> Integer
posSet (x : xs) s =
  if Set.member x s then abs $ fst x + abs (snd x) else posSet xs (Set.insert x s)

dist :: [Direction] -> Cartesian -> Integer -> Integer -> [Position] -> [Position]
dist [] c x y pos = pos
dist (d : ds) c x y pos = case updateDir (fst d) c of
  'N' -> dist ds 'N' (x + snd d) y ((x + snd d, y) : pos)
  'E' -> dist ds 'E' x (y + snd d) ((x, y + snd d) : pos)
  'W' -> dist ds 'W' x (y - snd d) ((x, y - snd d) : pos)
  'S' -> dist ds 'S' (x - snd d) y ((x - snd d, y) : pos)

updateDir :: Cartesian -> Cartesian -> Cartesian
updateDir turn curr =
  case curr of
    'N' -> (\x -> if x == 'L' then 'W' else 'E') turn
    'E' -> (\x -> if x == 'L' then 'N' else 'S') turn
    'W' -> (\x -> if x == 'L' then 'S' else 'N') turn
    'S' -> (\x -> if x == 'L' then 'E' else 'W') turn

parse :: String -> [Direction]
parse s =
  let line = fst $ line1 s
      list = splitOn " " line
   in map (\x -> (head x, read $ tail x)) list
