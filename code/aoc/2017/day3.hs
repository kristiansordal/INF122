import Data.List
import Data.Map (Map, mapAccum, mapMaybe)
import Data.Map qualified as Map

day3a :: Int -> Int
day3a n = a + abs ((n - b) `mod` (2 * a) - a)
  where
    (a, b) = last $ zip [1 ..] $ takeWhile (< n) $ scanl (+) 1 [8, 16 ..]

day3b :: Int -> Int
day3b n = head $ filter (> n) values
  where
    (_, values) = mapAccumL next (Map.singleton (0, 0) 1) positions
    next m pos = (Map.insert pos n m, n)
      where
        n = sum . mapMaybe ((`Map.lookup` m) . add pos) $ take 8 positions
    _ : positions = scanl add (0, 0) $ do
      n <- [0, 2 ..]
      replicate (n - 1) (0, -1)
        ++ replicate n (-1, 0)
        ++ replicate n (0, 1)
        ++ replicate (n + 1) (1, 0)
    add (a, b) (c, d) = (a + c, b + d)
