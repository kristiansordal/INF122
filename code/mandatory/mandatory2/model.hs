module Model
  ( TextModel,
    createModel,
    nextDistribution,
  )
where

import Control.Arrow
import Control.Monad
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import NGram

-- The type for our Markov process text model.
type TextModel = Map NGram (Map Char Weight, Weight)

-- The empty model with no n-grams.
emptyModel :: TextModel
emptyModel = Map.empty

-- Update a model with a new n-gram followed by a character.
increaseWeight :: NGram -> Char -> TextModel -> TextModel
increaseWeight ngram next = Map.insertWith (\(m1, t1) (m2, t2) -> (Map.unionWith (+) m1 m2, t1 + t2)) ngram (Map.singleton next 1, 1)

-- The distribution of next n-grams after a given one.
nextDistribution :: TextModel -> NGram -> Maybe ([(NGram, Weight)], Weight)
nextDistribution model current =
  case Map.lookup current model of
    Just (n, weight) -> Just (map (\(x, y) -> (updateGram current x, y)) (Map.toList n), weight)
    Nothing -> Nothing

createModel :: Integer -> String -> TextModel
createModel n = foldl' (flip $ uncurry increaseWeight) emptyModel . gramsWithNext n
