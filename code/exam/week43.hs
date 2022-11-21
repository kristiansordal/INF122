data RoseTree a = Branch a [RoseTree a]
  deriving (Eq, Show)

instance Functor RoseTree where
  fmap :: (a -> b) -> RoseTree a -> RoseTree b
  fmap f (Branch n []) = Branch (f n) []
  fmap f (Branch n ns) = Branch (f n) ((fmap . fmap) f ns)

sumNodes :: (Num a) => RoseTree [a] -> RoseTree a
sumNodes = fmap sum
