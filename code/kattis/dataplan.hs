import Control.Monad
import Data.List.Split

main = do
  plan <- getLine
  num <- getLine
  inputs <- replicateM (read num) getLine
  let inputs' = map read inputs :: [Int]
      ans = (read plan * length inputs' + read plan) - sum inputs'
  print ans
