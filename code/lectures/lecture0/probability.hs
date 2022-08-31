module Probability where

import Control.Monad
import Control.Monad.State
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ratio
import System.Random
import Prelude hiding (map)
import qualified Prelude (map)

{-
   This file implements finite, discrete probability
   distributions a la «Probabilistic functional programming in Haskell»

   Main difference from Numeric.Probability is that we use Map
   instead of assosiation lists, throwing out some convenience
   for the sake of efficiency.
-}

newtype Distribution d a = M (Map a d)

unM (M a) = a

instance (Show d, Ord a, Show a) => Show (Distribution d a) where
  show (M p) = "M " ++ show p

-- Take the naive sum of two distributions
(<+>) :: (Ord a, Num d) => Distribution d a -> Distribution d a -> Distribution d a
(M p) <+> (M p') = M $ Map.foldlWithKey' (\n a w -> Map.insertWith (+) a w n) p p'

-- Force an event to happen
force :: (Ord a, Num d) => a -> Distribution d a
force event = M $ Map.fromList [(event, 1)]

-- Conditional probability, observe an event and continue.
bind ::
  (Num d, Ord a, Ord b) =>
  Distribution d a ->
  (a -> Distribution d b) ->
  Distribution d b
bind (M p) f = Map.foldlWithKey' (\n a w -> n <+> (M $ Map.map (w *) (unM $ f a))) (M $ Map.empty) p

-- Uniform disitrubtion
uniform :: (Ord a, Fractional d) => [a] -> Distribution d a
uniform l = M $ Map.fromList $ zip l (cycle [1 / (fromInteger $ toInteger $ length l)])

-- Compute the probability of a given observation
p :: Fractional d => (a -> Bool) -> Distribution d a -> d
p b (M d) = (sum $ Prelude.map snd $ filter (b . fst) l) / (sum $ Prelude.map snd l)
  where
    l = Map.toList d

-- Map a function on all outcomes of a distribution
(!) :: (Ord a, Ord b, Fractional d) => Distribution d a -> (a -> b) -> Distribution d b
(M p) ! f = bind (M p) (\x -> uniform [f x])

-- Sample a distribution using a random generator
sample ::
  (Num p, Random p, Ord p, RandomGen g) =>
  Distribution p t ->
  g ->
  (t, g)
sample (M d) g =
  let (x, g') = randomR (0, sum ps) g
   in ( fst $
          head $
            dropWhile (\(a, p) -> p < x) $
              List.foldl' (\l (a, p) -> (a, p) : Prelude.map (\(a', p') -> (a', p + p')) l) [] q,
        g'
      )
  where
    q = (Map.toList d)
    ps = Prelude.map snd q

-- Make a bunch of samples
samples ::
  (Num p, Random p, Ord p, MonadState s m, RandomGen s) =>
  Int ->
  Distribution p b ->
  m [b]
samples n (M d) =
  let q = (Map.toList d)
      ps = Prelude.map snd q
      rs = List.foldl' (\l (a, p) -> (a, p) : Prelude.map (\(a', p') -> (a', p + p')) l) [] q
      total = sum ps
   in do
        xs <-
          sequence $
            replicate n $
              state $ randomR (0, total)
        return $
          fmap
            ( \x ->
                fst $
                  head $
                    dropWhile (\(a, p) -> p < x) $
                      rs
            )
            xs

-- Make a distributionfrom a list of occurences
frequencyList :: (Ord a, Fractional d) => [a] -> Distribution d a
frequencyList as =
  M $
    Map.fromList $
      Prelude.map (\l -> (head l, fromInteger $ toInteger $ length l)) $
        List.group $ List.sort as

-- Roll using the standard random generator
roll :: Distribution Double a -> IO a
roll = getStdRandom . sample

-- Dice
d4 :: (Fractional d) => Distribution d Integer
d6 :: (Fractional d) => Distribution d Integer
d8 :: (Fractional d) => Distribution d Integer
d10 :: (Fractional d) => Distribution d Integer
d12 :: (Fractional d) => Distribution d Integer
d20 :: (Fractional d) => Distribution d Integer
d4 = uniform [1 .. 4]

d6 = uniform [1 .. 6]

d8 = uniform [1 .. 8]

d10 = uniform [0 .. 9]

d12 = uniform [1 .. 12]

d20 = uniform [1 .. 20]
