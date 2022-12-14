data BinTree a
  = Empty
  | Branch (BinTree a) a (BinTree a)
  deriving (Show)

leaf x = Branch Empty x Empty

treeElem :: (Ord a) => a -> BinTree a -> Bool
treeElem x Empty = False
treeElem x (Branch less y greater)
  | x == y = True
  | x < y = treeElem x less
  | otherwise = treeElem x greater

insertSimple :: (Ord a) => a -> BinTree a -> BinTree a
insertSimple x Empty = leaf x
insertSimple x t@(Branch lesser y greater)
  | x == y = t
  | x < y = Branch (insertSimple x lesser) y greater
  | otherwise = Branch lesser y (insertSimple x greater)

rotateLeft :: BinTree a -> BinTree a
rotateLeft Empty = Empty
rotateLeft t@(Branch lesser a Empty) = t
rotateLeft (Branch lesser a (Branch between b greater)) =
  Branch (Branch lesser a between) b greater

rotateRight :: BinTree a -> BinTree a
rotateRight Empty = Empty
rotateRight t@(Branch Empty a greater) = t
rotateRight (Branch (Branch lesser a between) b greater) =
  Branch lesser a (Branch between b greater)
