module Week37Exercise0 where

-- zip, map, show, filter

namesAndAges :: [String] -> [Integer] -> [String]
namesAndAges names ages = zippedList
  where
    zippedList :: [(String, Integer)]
    zippedList = filter (zippedList . snd > 50) (zip names ages)
