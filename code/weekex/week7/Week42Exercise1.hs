module Week42Exercise1 where

import Data.Either

fromLeftAndRight :: (Either a b -> c) -> (a -> c, b -> c)
fromLeftAndRight f = (f . Left, f . Right)

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left x) = f x
either' _ f (Right y) = f y

toFstAndSnd :: (a -> (b, c)) -> (a -> b, a -> c)
toFstAndSnd f = ()

-- pair :: (a -> b) -> (a -> c) -> a -> (b, c)
-- pair x y z = (x, y)
