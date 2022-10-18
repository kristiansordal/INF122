module Week42Exercise1 where

import Data.Either

fromLeftAndRight :: (Either a b -> c) -> (a -> c, b -> c)
fromLeftAndRight f = (f . Left, f . Right)

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left x) = f x
either' _ f (Right y) = f y

split5 :: [a] -> ([a], [a])
split5 = splitAt 5

-- (take5, drop5) :: ([a] -> [a], [a] -> [a])
-- (take5, drop5) = toFstAndSnd split5
toFstAndSnd :: (a -> (b, c)) -> (a -> b, a -> c)
toFstAndSnd f = (fst . f, snd . f)

pair :: (a -> b) -> (a -> c) -> (a -> (b, c))
pair f g a = (f a, g a)
