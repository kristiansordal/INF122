factorial :: Integer -> Integer
factorial n = foldr (*) 1 [1 .. n]

head' :: [a] -> Maybe a
head' = foldr (\a b -> Just a) Nothing

map :: (a -> b) -> [a] -> [b]
-- map f = foldr (\a b -> f a : b) []
map f = foldr ((:) . f) []
