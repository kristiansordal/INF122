import Data.List.Split

main = do
  nums <- map read . splitOn " " <$> getLine :: IO [Double]
  print (head nums * head (tail nums) / 2)
