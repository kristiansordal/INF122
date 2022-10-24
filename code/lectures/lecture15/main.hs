import System.Environment

main = do
  args <- getArgs
  putStr $ unlines args
