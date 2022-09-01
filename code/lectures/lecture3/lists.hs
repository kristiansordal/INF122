module Lists where

intList :: Integer -> Integer -> [Integer]
intList x y = [x .. y]

stringList :: Char -> Char -> [Char]
stringList a b = [a .. b]

intListList :: Integer -> Integer -> [[Integer]]
intListList f g = [[f .. sum [f .. g]]]
