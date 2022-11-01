module NGram
  ( NGram,
    Weight,
    grams,
    gramsWithNext,
    combineGrams,
    updateGram,
  )
where

import Control.Arrow
import Control.Monad
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map

-- Rename types to clarify some type signatures later
type NGram = String

type Weight = Integer

-- Produce all n-grams contained in a given string
grams :: Integer -> String -> [NGram]
grams n s
  | length s >= fromIntegral n = take (fromIntegral n) s : grams n (tail s)
  | otherwise = []

-- Produce all n-grams contained in a given string, paired
-- with the subsequent character
gramsWithNext :: Integer -> String -> [(NGram, Char)]
gramsWithNext n s = zip (grams n s) (map last $ drop 1 $ grams n s)

-- -- Recombine a list of n-grams to a string
combineGrams :: [NGram] -> String
combineGrams n
  | not (null n) = head n ++ map last (tail n)
  | otherwise = []

-- -- Update an n-gram by adding a character to the end
-- -- and removing the first character.
updateGram :: NGram -> Char -> NGram
updateGram g c = tail g ++ [c]
