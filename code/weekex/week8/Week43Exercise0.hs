module Week43Exercise0 where

data BinSearchTree a
  = Empty
  | Branch (BinSearchTree a) a (BinSearchTree a)
  deriving (Eq, Show)

instance Foldable BinSearchTree where
  foldr _ z Empty = z
  foldr f z (Branch left node right) = foldr f (f node $ foldr f z right) left

toList :: BinSearchTree a -> [a]
toList = foldr (:) []
