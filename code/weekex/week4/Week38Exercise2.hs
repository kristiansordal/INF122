module Week38Exercise2 where

removeNothing :: [Maybe a] -> [a]
removeNothing [] = []
removeNothing (x : xs) =
  case x of
    Just x -> x : removeNothing xs
    Nothing -> removeNothing xs
