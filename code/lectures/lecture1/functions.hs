module Functions where

f :: Integer -> Integer
f x = x + 3

g :: Integer -> Integer -> Integer
g x y = x * (y + 1)

h z x = z (z x)

-- e) h (g 1) (f 7) = h (g 1) 10 = g 1 (g 1 10) = g 1 (1*(10+1)) = g 1 11 = 1 * (11+1) = 12

-- Think about it like this:
-- g 1 y = 1 * (y + 1) = y + 1
