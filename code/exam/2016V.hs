import System.IO
import System.Random

-- problem 2

append :: [a] -> [a] -> [a]
append = foldr (:)

type Name = String

type Pnum = Integer

type Pbook = Name -> Maybe Pnum

lookup :: Pbook -> Name -> Maybe Pnum
lookup b = b

insert :: Pbook -> Name -> Pnum -> Pbook
insert b n p
  | length (show p) > 8 = error "Invalid phone number format"
  | otherwise = \x -> if x == n then Just p else b x

delete :: Pbook -> Name -> Pbook
delete b n =
  case b n of
    Just num -> const Nothing
    Nothing -> b

main :: IO ()
main = do
  gen <- newStdGen
  let (num, gen') = randomR (10, 20) gen :: (Integer, StdGen)
  play num 1

play :: Integer -> Integer -> IO ()
play n p =
  if n < 3
    then do
      putStrLn $ "Player " ++ show p ++ " wins"
    else do
      putStrLn $ "Player " ++ show p ++ "'s turn"
      input <- getLine
      let remove = read input
      if remove > 0 && remove < 4
        then do
          putStrLn $ "Player " ++ show p ++ " removed " ++ show remove ++ " numbers from the heap"
          putStrLn $ "There are now " ++ show (n - remove) ++ " numbers in the heap"
          if p == 1
            then play (n - remove) 2
            else play (n - remove) 1
        else do
          putStrLn "Wrong input"
          play n p
