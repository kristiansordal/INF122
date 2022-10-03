import Prelude hiding (elem)

data AVLTree a
  = Empty
  | Branch BalanceFactor (AVLTree a) a (AVLTree a)
  deriving (Show)

data BalanceFactor
  = Leftheavy
  | Balanced
  | RightHeavy
  deriving (Eq, Show)

leaf :: a -> AVLTree a
leaf x = Branch Balanced Empty x Empty

height :: AVLTree a -> Integer
height Empty = 0
height (Branch _ l _ r) = 1 + max (height l) (height r)

size :: AVLTree a -> Integer
size Empty = 0
size (Branch _ l _ r) = 1 + size l + size r

elem :: (Ord a) => a -> AVLTree a -> Bool
elem x Empty = False
elem x (Branch _ less y greater)
  | x == y = True
  | x < y = elem x less
  | otherwise = elem x greater

-- insertSimple :: (Ord a) => a -> AVLTree a -> AVLTree a
-- insertSimple x Empty = leaf x
-- insertSimple x t@(Branch lesser y greater)
--   | x == y = t
--   | x < y = Branch (insertSimple x lesser) y greater
--   | otherwise = Branch lesser y (insertSimple x greater)

insert' :: (Ord a) => a -> AVLTree a -> (Bool, AVLTree a)
insert' x Empty = (True, leaf x)
-- insert' x (Branch bf lesserA a greaterA) =
insert' x (Branch LeftHeavy lesserA a greaterA) 
  | x < a = let (grew, newLesserA) = insert' x lesserA
            in if not grew
              then Branch LeftHeavy newLesserA a greaterA
              else case newLesserA of
              (Branch LeftHeavy lesserB b betweenBA) -> (Branch Balanced lesserB b (Branch Balanced betweenBA a greaterA))
              (Branch RightHeacy lesserB b (Branch cbf betweenBC c betweenCA)) -> (Branch Balanced (Branch (if cbf == RightHeavy then LeftHeavy else Balanced) lesserB b betweenBC) c (Branch (if cbf == LeftHeavy then RightHeavy else Balanced) betweenCA a greaterA))

  | x > a = let (grew, newGreaterA) = insert' x greaterA
    in if grew
      then (False, Branch Balanced lesserA a newGreaterA)
      else ()
  | otherwise = 
insert' x (Branch Balanced lesserA a greaterA) =
insert' x (Branch RightHeavy lesserA a greaterA) =

insert :: (Ord a) => a -> AVLTree a -> AVLTree a
insert x t = snd (insert' x t)

rotateLeft :: AVLTree a -> AVLTree a
rotateLeft Empty = Empty
rotateLeft t@(Branch _ lesser a Empty) = t
rotateLeft (Branch lesser a (Branch between b greater)) =
  Branch (Branch lesser a between) b greater

rotateRight :: AVLTree a -> AVLTree a
rotateRight Empty = Empty
rotateRight t@(Branch Empty a greater) = t
rotateRight (Branch (Branch lesser a between) b greater) =
  Branch lesser a (Branch between b greater)
