import Data.Array
import Data.List.Split

main = do
  -- file <- readFile "day2.in"
  -- let list = splitOn "," file
  --     list' = init $ init list
  --     nums :: [Integer] = map read list
  --     array = listArray (0, length nums - 1) nums

  nums <- map read . splitOn "," <$> getLine :: IO [Int]
  let array = listArray (0, length nums - 1) nums
      array' = array // [(1, 12), (2, 2)]
      target = 19690720
      result = head [100 * v1 + v2 | v1 <- [0 .. 99], v2 <- [0 .. 99], target == process 0 (array' // [(1, v1), (2, v2)])]
      ansP1 = process 0 array'

  print ansP1
  print result

-- process :: Array i e -> Array i e ->
process pc arr =
  let val = case arr ! pc of
        1 -> Just $ (arr ! (arr ! (pc + 1))) + (arr ! (arr ! (pc + 2)))
        2 -> Just $ (arr ! (arr ! (pc + 1))) * (arr ! (arr ! (pc + 2)))
        99 -> Nothing
      loc = arr ! (pc + 3)
   in maybe (arr ! 0) (\v -> process (pc + 4) (arr // [(loc, v)])) val

-- process pc arr = let fetch idx = arr ! (arr ! (pc + idx))
--                   value = (\op -> runOp op (fetch 1) (fetch 2)) <$> getOp (arr ! pc)
--                   loc = arr ! (pc + 3)
--                 in maybe (arr ! 0) (\v -> process (pc + 4) (arr // [(loc, v)])) value
