module Examples where

-- singleton :: t -> [t]
-- singleton element = [element]

-- f :: a -> a -> a
-- f a b = a

-- g :: a -> [[a]]
-- g a = [[a], [a]]

t :: (a -> b) -> (b -> c) -> a -> c
t f g a = g (f a)

s :: (a -> b -> c) -> (a -> b) -> a -> c
s f g a = f a (g a)
