import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as Set

type Graph n = Map n (Set n)

insertEdge :: (Ord n) => n -> n -> Graph n -> Graph n
insertEdge n1 n2 g = Map.insertWith Set.union n2 Set.empty $ Map.insertWith Set.union n1 (Set.singleton n2) g

insertEdge' :: (Ord n) => n -> n -> Graph n -> Graph n
insertEdge' n1 n2 = Map.insertWith Set.union n2 Set.empty . Map.insertWith Set.union n1 (Set.singleton n2)

disjoint :: (Ord a) => Set a -> Set a -> Bool
disjoint s1 = Set.null . Set.intersection s1

hasCycle :: (Ord n) => Graph n -> n -> Bool
hasCycle g n = path g n Set.empty

path :: (Ord n) => Graph n -> n -> Set n -> Bool
path g s visited =
  case Map.lookup s g of
    Just neighbours ->
      disjoint visited neighbours
        && ( let visited' = Set.insert s visited
              in any (\n -> path g n visited') neighbours
           )
    _ -> False

-- hasCycle :: (Ord node) => Graph node -> node -> Bool
-- hasCycle g start = path g start (Set.singleton start)

-- path :: (Ord n) => Graph n -> n -> Set n -> Bool
-- path g start visited
--   | isNothing $ Map.lookup start g = False
--   | otherwise =
--     case Map.lookup start g of
--       Just neighbors ->
--         not (disjoint visited neighbors)
--           || ( do
--                  let visited' = Set.insert start visited
--                  any (\n -> path g n visited') neighbors
--              )
--       _ -> False
