module Week38Exercise2 where

removeNothing :: [Maybe a] -> [a]
removeNothing [Nothing] = [Nothing]
removeNothing (x : xs) = removeNothing
