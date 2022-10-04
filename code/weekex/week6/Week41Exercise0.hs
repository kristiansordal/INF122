module Week41Exercise0 where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

type Graph n = Map n (Set n)

insertEdge :: (Ord n) => n -> n -> Graph n -> Graph n
insertEdge n1 n2 = Map.insertWith Set.union n2 Set.empty . Map.insertWith Set.union n1 (Set.singleton n2)
