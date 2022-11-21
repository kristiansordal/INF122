import Data.Map (Map)
import Data.Map qualified as Map

data Expr a
  = Var a
  | Lit Integer
  | Mul (Expr a) (Expr a)
  | Add (Expr a) (Expr a)
  deriving (Eq, Show)

eval :: (Ord variable, Num value) => Expr variable -> Map variable value -> Maybe value
eval (Lit a) map = Just (fromIntegral a)
eval (Var a) map = Map.lookup a map
eval (Mul x y) map = eval x map >>= \n -> eval y map >>= \m -> Just (n * m)
eval (Add x y) map = eval x map >>= \n -> eval y map >>= \m -> Just (n + m)
