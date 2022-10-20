{-# LANGUAGE InstanceSigs #-}

module Week43Exercise1 where

data RoseTree a = Branch a [RoseTree a]
  deriving (Eq, Show)

instance Functor RoseTree where
  fmap :: (a -> b) -> f a -> f b
