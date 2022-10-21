module Week43Exercise1 where

data RoseTree a = Branch a [RoseTree a]
  deriving (Eq, Show)

instance Functor RoseTree where
  fmap f (Branch a []) = Branch (f a) []
  fmap f (Branch a roseTrees) = Branch (f a) ((fmap . fmap) f roseTrees)

sumNodes :: (Num a) => RoseTree [a] -> RoseTree a
sumNodes = fmap sum
