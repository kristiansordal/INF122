lengthSum :: (Num a, Num b) => [a] -> (b, a)
lengthSum = foldr (\n (x, y) -> (1 + x, n + y)) (0, 0)

-- lengthSum (x : xs) = (fromIntegral $ length (x : xs), foldr (+) x xs)

inList :: (Eq a) => a -> [a] -> Bool
inList x = foldl (\acc y -> (x == y) || acc) False

data Expr = V Int | M Expr Expr | D Expr Expr

eval :: Expr -> Maybe Int
eval (V x) = Just x
eval (M r l) = do
  -- eval r  eval l
  -- let x =<< eval r
  -- y <- eval l
  return (Just x * y)
eval (D r l) = do
  x <- eval r
  y <- eval l
  x / y
