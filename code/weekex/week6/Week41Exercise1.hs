module Week41Exercise1 where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

type Graph n = Map n (Set n)

disjoint :: (Ord a) => Set a -> Set a -> Bool
disjoint n1 n2 = null (n1 `Set.intersection` n2)

hasCycle :: (Ord a) => Graph n -> n -> Bool
