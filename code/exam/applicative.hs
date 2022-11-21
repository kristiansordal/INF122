import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

type Graph n = Map n (Set n)

main = do
  let list = [Just (* 3), Just (+ 3)]
      nums = [Just 3, Just 3]
      list' = genFun [1 .. 10]
      num = genMaybe [1 .. 10]
      nums' = zipWith (<*>) list' num
      map = Map.insertWith (++) 5 [1] (Map.fromList [(5, [1]), (3, [2])])
      graph = Map.empty
      -- graph'' = map (\x -> )
      graph'' = insertMany graph 1 [2 .. 10]
      graph''' = insertMany graph'' 4 [10 .. 100]
      graph'''' = insertMany graph''' 5 [101 .. 110]
      graph''''' = insertMany graph'''' 2 [111 .. 121]
      neighbors = getNeighbours graph''''' 2
      leaf = dfs graph''''' 1 Set.empty

  print graph'''''
  print neighbors
  print leaf

genMaybe :: (Num a) => [a] -> [Maybe a]
genMaybe = map Just

genFun :: (Num a) => [a] -> [Maybe (a -> a)]
genFun = map (\x -> Just (+ x))

-- insert many neigbours into the graph
insertMany :: (Ord n) => Graph n -> n -> [n] -> Graph n
insertMany g n = foldr (edgeBetween n) g

-- insert undirected edge
edgeBetween :: (Ord n) => n -> n -> Graph n -> Graph n
edgeBetween n1 n2 = Map.insertWith Set.union n2 (Set.singleton n1) . Map.insertWith Set.union n1 (Set.singleton n2)

-- get the neighbours of a node
getNeighbours :: (Ord n) => Graph n -> n -> Maybe [n]
getNeighbours g n = case Map.lookup n g of
  (Just set) -> Just (Set.toList set)
  _ -> Nothing

dfs :: (Ord n) => Graph n -> n -> Set n -> n
dfs g r v = case Map.lookup r g of
  (Just set) ->
    let next = head $ Set.toList set
     in if Set.member next v
          then next
          else dfs g next (Set.insert next v)
  Nothing -> r
