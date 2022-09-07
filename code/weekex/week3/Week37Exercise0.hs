module Week37Exercise0 where

namesAndAges :: [String] -> [Integer] -> [String]
namesAndAges names ages = [name ++ " is " ++ year ++ " years old" | (name, year) <- stringList]
  where
    filteredList = filter (\(_, age) -> age <= 50) (zip names ages)
    stringList = [(name, show year) | (name, year) <- filteredList]
