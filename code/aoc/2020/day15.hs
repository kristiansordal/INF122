import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

main = do
  let input = [18, 11, 9, 0, 5, 1]
      initMap = createInitMap Map.empty (take 5 input) 1
      ansP1 = memoryGame initMap (last input) (fromIntegral $ length input + 1)
  print ansP1

memoryGame :: Map Integer Integer -> Integer -> Integer -> Integer
memoryGame ages prev 100000 = prev
memoryGame ages prev turn =
  case Map.lookup prev ages of
    Just prevAge -> memoryGame (Map.insert prev (turn - 1) ages) (turn - 1 - prevAge) (turn + 1)
    Nothing -> memoryGame (Map.insert prev (turn - 1) ages) 0 (turn + 1)

createInitMap :: Map Integer Integer -> [Integer] -> Integer -> Map Integer Integer
createInitMap a [] i = a
createInitMap a (x : xs) i = createInitMap (Map.insert x i a) xs (i + 1)

-- memoryGame :: Ages -> Prev -> Turn -> IO Integer
-- memoryGame ages prev 100000 = return prev
-- memoryGame ages prev turn =
--   case Map.lookup prev ages of
--     Just prevAge -> do
--       print ("(J) Turn: " ++ show turn ++ " prev: " ++ show prev)
--       -- print (Map.toList ages)
--       memoryGame (Map.insert prev (turn - 1) ages) (turn - 1 - prevAge) (turn + 1)
--     Nothing -> do
--       print ("(N) Turn: " ++ show turn ++ " prev: " ++ show prev)
--       -- print (Map.toList ages)
--       memoryGame (Map.insert prev (turn - 1) ages) 0 (turn + 1)
