module Week40Exercise2 where

data BinSearchTree a
  = Empty
  | Branch (BinSearchTree a) a (BinSearchTree a)
  deriving (Eq, Show)

toList :: (Eq a) => BinSearchTree a -> [a]
toList Empty = []
toList (Branch Empty a Empty) = [a]
toList (Branch lesser a greater) =
  toList lesser ++ [a] ++ toList greater
