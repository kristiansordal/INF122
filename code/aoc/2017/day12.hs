import Data.List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import Text.Regex

main = do
  input <- map parse . lines <$> readFile "day12.in"
  let g = create input Map.empty
  let nums = map (\x -> Set.toList (dfs g x Set.empty [x])) (concat input)
  let sets = map (Set.toList . Set.fromList) nums
  let uniqueSets = map Set.fromList (nub nums)
  let all = concat (filter (elem 0) nums)

  print uniqueSets
  let g = groups uniqueSets 1
  print g

type Graph n = Map n (Set n)

groups :: (Show n, Ord n) => [Set n] -> Integer -> Integer
groups [] g = g
groups (x : xs) g =
  let group = filter (disjoint x) xs
      rest = filter (`elem` group) (x : xs)
   in if null rest
        then do
          g
        else do
          groups rest (g + 1)

dfs :: (Show n, Ord n) => Graph n -> n -> Set n -> [n] -> Set n
dfs graph node visited toSearch =
  let visited' = Set.insert node visited
   in case Map.lookup node graph of
        Just neighbours ->
          let toSearch' = toSearch ++ [x | x <- Set.toList neighbours, x `notElem` Set.toList visited']
           in do
                if null toSearch'
                  then visited'
                  else dfs graph (head toSearch') visited' (tail toSearch')
        Nothing -> visited

disjoint :: (Ord a) => Set a -> Set a -> Bool
disjoint s1 = Set.null . Set.intersection s1

create :: (Ord n) => [[n]] -> Graph n -> Graph n
create xs g = foldl (\g x -> insertMany g (head x) (tail x)) g xs

-- insert many neigbours into the graph
insertMany :: (Ord n) => Graph n -> n -> [n] -> Graph n
insertMany g n = foldr (edgeBetween n) g

-- insert undirected edge
edgeBetween :: (Ord n) => n -> n -> Graph n -> Graph n
edgeBetween n1 n2 = Map.insertWith Set.union n2 (Set.singleton n1) . Map.insertWith Set.union n1 (Set.singleton n2)

parse :: String -> [Integer]
parse s = map read (filter (/= "") (splitRegex (mkRegex "[^0-9]") s))
