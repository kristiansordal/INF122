module Week41Exercise1 where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

type Graph n = Map n (Set n)

disjoint :: (Ord a) => Set a -> Set a -> Bool
disjoint n1 n2 = null (n1 `Set.intersection` n2)

hasCycle :: (Ord n) => Graph n -> n -> Bool
hasCycle g node =
  if disjoint visited any neighbours
    then hasCycle g node
    else True
  where
    neighbours = Map.lookup node g
    visited = visitedNodes neighbours (Set.singleton node)

visitedNodes :: (Ord a) => a -> Set a -> Set a
visitedNodes = Set.insert
