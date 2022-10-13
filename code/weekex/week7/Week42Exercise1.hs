module Week42Exercise1 where

import Data.Either

fromLeftAndRight :: (Either a b -> c) -> (a -> c, b -> c)
fromLeftAndRight f = (f . Left, f . Right)

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left x) = f x
either' _ f (Right y) = f y

toFstAndSnd :: (a -> (b, c)) -> (a -> b, a -> c)
-- toFstAndSnd f a = (fst $ f a, snd $ f a)
toFstAndSnd f a = f ((fst a), (fst a))

pair :: (a -> b) -> (a -> c) -> (a -> (b, c))
pair f g a = (f a, g a)
