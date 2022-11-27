import Control.Monad
import Data.List.Extra

main = do
  input <- getLine
  let info = map read (splitOn " " $ fst (line1 input)) :: [Integer]
  nums <- replicateM (fromIntegral $ head info) getLine
  putStrLn (potions (map read nums) (fromIntegral $ last info))

potions :: [Integer] -> Integer -> String
potions l x
  | fromIntegral (ceiling $ (maximum l) / x) <= length l = "NO"
  | otherwise = "YES"
