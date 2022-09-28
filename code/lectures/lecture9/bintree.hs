data BinTree a = Empty
  | Branch (BinTree a) a (BinTree a)

leaf x = Branch Empty x Empty

elem :: (Ord a) => a -> BinTree a -> Bool
elem x Empty = False
elem x (Branch less y greater)
  = if x == y
  then true
  else if x < y
      then 
