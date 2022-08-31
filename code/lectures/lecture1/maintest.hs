module Main where

f x = x + 3

main = do
  putStrLn "Hva heter du?"
  name <- getLine
  putStrLn ("Hei, " ++ name)
