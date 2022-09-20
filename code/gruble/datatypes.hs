data NonEmptyList a
  = Last a
  | Cons a (NonEmptyList a)

minList :: Ord a => NonEmptyList a -> a
minList (Last x) = x
minList (Cons x xs) = min x (minList xs)

data Tree a
  = Node a
  | Branch (Tree a, Tree a)
  deriving (Show)

x :: Tree Int
x = Branch (Node 1, Node 2)

y :: Tree Int
y = Branch (Branch (Node 1, Node 2), Node 3)

z :: Tree Int
z = Node 4

treeMin :: Ord a => Tree a -> a
treeMin (Node x) = x
treeMin (Branch (left, right)) = min (treeMin left) (treeMin right)

minMaxList :: Ord a => NonEmptyList a -> (a, a)
minMaxList (Last x) = (x, x)
minMaxList (Cons x xs) =
  let (x1, x2) = minMaxList xs
   in (min x x1, max x x2)

data Side = L | R deriving (Show)

type Path = [Side]

treeMinPath :: Ord a => Tree a -> (a, Path)
treeMinPath (Node x) = (x, [])
treeMinPath (Branch (left, right)) =
  let (leftMin, leftPath) = treeMinPath left
      (rightMin, rightPath) = treeMinPath right
   in if leftMin < rightMin
        then (leftMin, L : leftPath)
        else (rightMin, R : rightPath)
