lengthSum :: (Num a, Num b) => [a] -> (b, a)
lengthSum = foldr (\n (x, y) -> (1 + x, n + y)) (0, 0)

-- lengthSum (x : xs) = (fromIntegral $ length (x : xs), foldr (+) x xs)

inList :: (Eq a) => a -> [a] -> Bool
inList x = foldl (\acc y -> (x == y) || acc) False

data Expr = V Int | M Expr Expr | D Expr Expr

div' :: Int -> Int -> Maybe Int
div' _ 0 = Nothing
div' x y = Just (x `div` y)

eval :: Expr -> Maybe Int
eval (V x) = Just x
eval (M r l) =
  case eval r of
    Nothing -> Nothing
    Just x -> case eval l of
      Nothing -> Nothing
      Just y ->
        if x >= 0 && y >= 0
          then Just (x * y)
          else Nothing
eval (D r l) =
  case eval r of
    Nothing -> Nothing
    Just x -> case eval l of
      Nothing -> Nothing
      Just y -> div' x y

toDoList :: [IO a] -> IO [a]
toDoList [] = return []
toDoList (x : xs) = do
  action <- x
  list' <- toDoList xs
  return (action : list')

mapActions :: (a -> IO b) -> [a] -> IO [b]
mapActions f [] = return []
mapActions f (x : xs) = do
  y <- f x
  ys <- mapActions f xs
  return (y : ys)
