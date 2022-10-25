{-# LANGUAGE FlexibleInstances #-}

module Week43Exercise2 where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set

class IntegerGraph g where
  emptyGraph :: g
  insertNode :: Integer -> g -> g
  insertEdge :: Integer -> Integer -> g -> g
  nodeInGraph :: Integer -> g -> Bool
  edgeInGraph :: Integer -> Integer -> g -> Bool

type MyGraph = Map Integer (Set Integer)

instance IntegerGraph MyGraph where
  emptyGraph = Map.empty
  insertEdge n1 n2 = Map.insertWith Set.union n2 Set.empty . Map.insertWith Set.union n1 (Set.singleton n2)
  insertNode n = Map.insertWith Set.union n Set.empty
  nodeInGraph n = Map.member n
  edgeInGraph n1 n2 g =
    case Map.lookup n1 g of
      Just set -> Set.member n2 set
      Nothing -> False

graph :: (IntegerGraph g) => g
graph =
  do
    let g1 = insertEdge 5 1 g
    let g2 = insertEdge 5 8 g1
    let g3 = insertEdge 8 5 g2
    let g4 = insertEdge 1 8 g3
    let g5 = insertEdge 1 6 g4
    g5
  where
    empty = emptyGraph
    g =
      insertNode 1 $
        insertNode 3 $
          insertNode 5 $
            insertNode 8 $
              insertNode 6 $
                insertNode 3 empty
