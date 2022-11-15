import Data.List.Split
import Data.Set qualified as Set

main = do
  file <- readFile "day6.in"
  let answers = map (splitOn "\n") (splitOn "\n\n" file)
      ansP1 = allSame answers
  print ansP1

allSame :: [[String]] -> Int
allSame = sum . map (Set.size . Set.fromList . concat . dropWhile (== " "))
