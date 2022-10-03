data Expr
  = Const Integer
  | Add Expr Expr
  | Mul Expr Expr

ex0 :: Expr
ex0 = Add (Mul (Const 3) (Const 4)) (Const 7)

eval :: Expr -> Integer
eval (Const value) = value
-- eval (Add exprLeft exprRight) = eval exprLeft + eval exprRight
eval (Add exprLeft exprRight) = max (eval exprLeft) (eval exprRight)
eval (Mul exprLeft exprRight) = eval exprLeft * eval exprRight
