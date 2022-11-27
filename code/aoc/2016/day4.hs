import Data.List.Extra
import Data.Maybe

main = do
  input <- map parse . lines <$> readFile "day4.in"
  -- let ansP1 = getReal input

  print input

-- print ansP1

type Room = ((Integer, String), String)

-- getReal :: [Room] -> Integer
-- getReal r = sum (mapMaybe realRoom r)

-- realRoom :: Room -> Maybe (String, Integer)
realRoom :: Room -> Either String Integer
realRoom r =
  let check = snd $ fst r
   in if sort check == sort (take 5 (fiveCommon (snd r) []))
        then Right (fst $ fst r)
        else Left (fiveCommon (snd r) [])

fiveCommon :: String -> String -> String
fiveCommon [] c = c
fiveCommon s c =
  let char = mostFrequent s
      newStr = filter (/= char) s
   in fiveCommon newStr (c ++ [char])

mostFrequent :: String -> Char
mostFrequent ns =
  snd (maximum [(length ks, head ks) | ks <- group (sort ns)])

getCount :: Char -> String -> Integer
getCount c = fromIntegral . length . filter (== c)

parse :: String -> Room
parse str =
  let s = splitOn "-" (fst $ line1 str)
      check = splitOn "[" (last s)
      num = head check
   in ((read num, init $ last check), concat $ init s)
