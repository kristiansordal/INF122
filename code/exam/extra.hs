module Extra where

-- import Prelude hiding (fmap)

doubleNames :: [String] -> [String]
doubleNames [] = []
doubleNames list = [x ++ "-" ++ y | x <- list, y <- list, head x /= head y]

takeMaybe :: Integer -> [a] -> Maybe [a]
takeMaybe 0 _ = Just []
takeMaybe x list
  | x < 0 = Nothing
  | fromIntegral x < len = Nothing
  | fromIntegral x == len = Just (take len list)
  | otherwise = Nothing
  where
    len = length (take (fromIntegral x) list)

data BinSearchTree a
  = Empty
  | Branch (BinSearchTree a) a (BinSearchTree a)
  deriving (Eq, Show)

instance Functor BinSearchTree where
  fmap :: (a -> b) -> BinSearchTree a -> BinSearchTree b
  fmap f (Branch Empty n Empty) = Branch Empty (f n) Empty
  fmap f (Branch l n r) = Branch (fmap f l) (f n) (fmap f r)
