sorted :: (ord a) => [a] -> Bool
sorted [] = True
sorted (x : []) = True
-- sorted (x : y : ys) = (x <= y) && sorted (y : ys)
-- Alternativelt
sorted (x : y : ys) =
  if (x <= y)
    then sorted (y : ys)
    else False
