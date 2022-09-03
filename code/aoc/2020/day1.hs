module Day1 where

readInt :: String -> Int
readInt = read

twoSum :: Int -> [Int] -> [(Int, Int)]
twoSum sum nums =
  let ixs = zip [0 ..] nums
   in ixs
        >>= ( \(i, x) ->
                ixs >>= \(j, y) -> [(i, j) | (x + y) == sum]
            )

main :: IO ()
main =
  do
    input <- readFile "inputday1.txt"
    print . map readInt . words $ input
    let nums = map readInt . words $ input
    mapM_ print $ twoSum 2020 nums
