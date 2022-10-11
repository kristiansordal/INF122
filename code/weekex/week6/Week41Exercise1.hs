module Week41Exercise1 where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set

type Graph n = Map n (Set n)

disjoint :: (Ord a) => Set a -> Set a -> Bool
disjoint n1 n2 = null (n1 `Set.intersection` n2)

hasCycle :: (Ord node) => Graph node -> node -> Bool
hasCycle g start = path g start (Set.singleton start)

path :: (Ord n) => Graph n -> n -> Set n -> Bool
path g start visited
  | isNothing $ Map.lookup start g = False
  | otherwise =
    case Map.lookup start g of
      Just neighbors ->
        not (disjoint visited neighbors)
          || ( do
                 let visited' = Set.insert start visited
                 any (\n -> path g n visited') neighbors
             )
      _ -> False
