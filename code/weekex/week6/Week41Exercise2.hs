module Week41Exercise2 where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe

data Expr a
  = Var a
  | Lit Integer
  | Mul (Expr a) (Expr a)
  | Add (Expr a) (Expr a)
  deriving (Eq, Show)

eval :: (Ord variable, Num value) => Expr variable -> Map variable value -> Maybe value
eval (Lit a) map = Just (fromIntegral a)
eval (Var a) map = Map.lookup a map
eval (Add a b) map =
  eval a map >>= \n ->
    eval b map >>= \m ->
      Just (n + m)
eval (Mul a b) map =
  eval a map >>= \n ->
    eval b map >>= \m ->
      Just (n * m)
