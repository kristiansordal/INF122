class Functor f where
  fmap :: (a -> b) -> f a -> f b

data Maybe a
  = Nothing
  | Just a

-- instance Functor Maybe
--   -- fmap :: (a -> b) -> Maybe a -> Maybe b
--   fmap f Nothing = Nothing
--   fmap f (Just a) = Just (f a)

-- mapMaybe :: (a -> b) -> Maybe a -> Maybe b
-- mapMaybe f Nothing = Nothing
-- mapMaybe f (Just a) = Just (f a)

-- addThreeMaybe = mapMaybe (+ 3)

-- map :: (a -> b) -> [a] -> [b]

instance Functor (Either e) where
  -- fmap :: (a -> b) -> Either e a -> Either e b
  fmap f (Left e) = Left e
  fmap f (Right a) = Right (f a)

-- Vi ser at lovene holder:
-- fmap id (left e) = Left e (== id (Left e))
-- fmap id (Right a) = Right (id a) = RIght a ( == id (Right a))
