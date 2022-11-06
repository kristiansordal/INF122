data BT
  = B Int
  | N BT Int BT

harEl :: (t -> Bool) -> [t] -> Bool
harEl = any

el :: (t -> Bool) -> [t] -> t
el pr = head . filter pr

gRep :: (t -> Bool) -> t -> [t] -> [t]
gRep pr y =
  map
    ( \x ->
        case () of
          _
            | pr x -> y
            | otherwise -> x
    )

elt :: BT -> Int -> Bool
elt (B val) x = val == x
elt (N left val right) x
  | val == x = True
  | val /= x = elt left x || elt right x

toL :: BT -> [Int]
toL (B x) = [x]
toL (N left val right) = toL left ++ [val] ++ toL right

dup :: BT -> Bool
dup = dupL . toL

dupL :: [Int] -> Bool
dupL [] = False
dupL (x : xs) = elem x xs || dupL xs

naboL :: (Eq t) => [(t, t)] -> [(t, [t])]
naboL [] = []
naboL (x : xs) = (fst x, getNeighbours (fst x) (x : xs)) : naboL xs

naboL' :: (Eq t) => [(t, [t])] -> [(t, [t])]
naboL' [] = []
naboL' (x : xs)
  | fst x `elem` map fst xs = naboL' xs
  | otherwise = x : naboL' xs

getNeighbours :: (Eq t) => t -> [(t, t)] -> [t]
getNeighbours x list = map snd list'
  where
    list' = filter ((== x) . fst) list
