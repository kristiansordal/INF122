import Data.List.Extra
import Data.Set (Set)
import Data.Set qualified as Set

main = do
  input <- map parse . lines <$> readFile "day4.in"
  let valids = getValidCount input
      strToNum = map (map stringToList) input
      valids' = getValidCount strToNum

  print valids
  print strToNum

getValidCount :: (Ord a) => [[a]] -> Integer
getValidCount s = fromIntegral $ length (filter (== True) (map (`valid'` Set.empty) s))

valid' :: (Ord a) => [a] -> Set a -> Bool
valid' [] s = True
valid' (x : xs) s = not (Set.member x s) && valid' xs (Set.insert x s)

stringToList :: String -> Integer
stringToList l = sum (map alphabetIndex l)

alphabetIndex :: Char -> Integer
alphabetIndex s = fromIntegral $ head $ elemIndices s "abcdefghijklmnopqrstuvwxyz"

parse :: String -> [String]
parse s =
  let w = fst $ line1 s
   in splitOn " " w
