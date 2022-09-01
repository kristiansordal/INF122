data Parity = Even | Odd
  deriving (Eq, Show)

data SortedList a
  = Empty
  | Cons Parity a (SortedList a)
  deriving (Eq, Show)

flipParity :: Parity -> Parity
flipParity Even = Odd
flipParity Odd = Even

parity :: SortedList a -> Parity
parity Empty = Even
parity (Cons p _ _) = p
  -- | a <= b = Cons (flipParity p) a (Cons p b bs)
  -- | otherwise = Cons (flipParity p) b (insert a bs)

insert :: (Ord a) => a -> SortedList a -> SortedList a
insert a Empty = Cons a Empty
insert a (Cons p b bs)
  | a <= b = Cons a (Cons p b bs)
  | otherwise = Cons b (insert p a bs)
