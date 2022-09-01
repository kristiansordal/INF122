import Prelude hiding (elem)

data AVLTree a
  = Empty
  | Branch (AVLTree a) a (AVLTree a)
  deriving (Show)

data BalanceFactor
  = Leftheavy
  | Balanced
  | RightHeavy
  deriving (Eq, Show)

leaf x = Branch Empty x Empty

elem :: (Ord a) => a -> AVLTree a -> Bool
elem x Empty = False
elem x (Branch _ less y greater)
  | x == y = True
  | x < y = elem x less
  | otherwise = elem x greater

insertSimple :: (Ord a) => a -> AVLTree a -> AVLTree a
insertSimple x Empty = leaf x
insertSimple x t@(Branch lesser y greater)
  | x == y = t
  | x < y = Branch (insertSimple x lesser) y greater
  | otherwise = Branch lesser y (insertSimple x greater)

rotateLeft :: AVLTree a -> AVLTree a
rotateLeft Empty = Empty
rotateLeft t@(Branch lesser a Empty) = t
rotateLeft (Branch lesser a (Branch between b greater)) =
  Branch (Branch lesser a between) b greater

rotateRight :: AVLTree a -> AVLTree a
rotateRight Empty = Empty
rotateRight t@(Branch Empty a greater) = t
rotateRight (Branch (Branch lesser a between) b greater) =
  Branch lesser a (Branch between b greater)
