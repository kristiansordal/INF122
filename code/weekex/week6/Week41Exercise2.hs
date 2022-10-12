module Week41Exercise2 where

import Data.Map (Map)
import qualified Data.Map as Map
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
eval (Add a b) map = do
  left <- eval a map
  right <- eval b map
  return (left + right)
eval (Mul a b) map = do
  left <- eval a map
  right <- eval b map
  return (left * right)
