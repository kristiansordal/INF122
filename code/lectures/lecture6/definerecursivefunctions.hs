import Prelude hiding (map, reverse, zip, (++))

zip :: [a] -> [b] -> [(a, b)]
zip [] _ = []
zip _ [] = []
zip (a : as) (b : bs) = (a, b) : zip as bs

map :: (a -> b) -> [a] -> [b]
map f [] = []
map g (a : as) = f a : map f as

(++) :: [a] -> [a] -> [a]
[] ++ bs = bs
(a : as) ++ bs = a : (as ++ bs)

reverse' :: [a] -> [a] -> [a]
reverse' [] bs = bs
reverse' (a : as) bs = reverse' as (a : bs)

reverse :: [a] -> [a]
