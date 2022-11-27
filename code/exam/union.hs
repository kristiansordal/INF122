import Data.Map (Map)
import Data.Map qualified as Map

data Record key value
  = Record (Map key (Either (Record key value) value))

instance (Show key, Show value) => Show (Record key value) where
  show (Record m) =
    "{"
      ++ concat
        ( intersperse
            ";"
            [ show k ++ ":" ++ either show show v
              | (k, v) <- Map.toList m
            ]
        )
      ++ "}"

-- a)
shallowUnion ::
  (Ord key) =>
  Record key value ->
  Record key value ->
  Record key value
shallowUnion (Record r1) = Map.union

-- b)
deepUnion ::
  (Ord key) =>
  Record key value ->
  Record key value ->
  Record key value
deepUnion (Record k1) (Record k2) = Map.unionWith (Map.insert) (Record k1) (Record k2)
