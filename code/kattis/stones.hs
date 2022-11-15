main = do
  input <- getLine
  let inp = read input
  (if even inp then putStrLn "Bob" else putStrLn "Alice")
