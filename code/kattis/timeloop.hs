import Control.Monad

main = do
  input <- getLine
  let num = read input :: Int
  abra num

abra :: Int -> IO ()
abra num = forM_ [1 .. num] $ \i -> putStrLn (show i ++ " Abracadabra")
