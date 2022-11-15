import Data.Set (Set)
import Data.Set qualified as Set

mengde :: (Eq t) => [t] -> Bool
mengde [] = False
mengde (x : xs) = x `notElem` xs

rep :: (Eq t) => [t] -> [t]
rep [] = []
rep (x : xs)
  | mengde (x : xs) = x : rep xs
  | otherwise = rep xs

eq :: (Eq t) => [t] -> [t] -> Bool
eq l1 l2 = all ((== True) . (\x -> x `elem` rep l2)) l1

qs :: [Int] -> [Int]
qs [] = []
qs (x : xs) = qs lower ++ [x] ++ qs higher
  where
    lower = [y | y <- xs, y < x]
    higher = [z | z <- xs, z >= x]

qs' :: [Int] -> [Int]
qs' [] = []
qs' (x : xs) =
  let lower = [y | y <- xs, y < x]
      higher = [z | z <- xs, z >= x]
   in qs' lower ++ [x] ++ qs' higher

half :: Int -> Maybe Int
half x
  | even x = Just (x `div` 2)
  | otherwise = Nothing

maybeSomething :: Maybe Int -> Maybe Int
maybeSomething x = x >>= half

greet :: IO String
greet = putStrLn "Hello, whats your name? " >> getLine >>= \name -> putStrLn ("Nice to meeat you " ++ name) >> return name

-- kattis oppgave
main :: IO ()
main = do
  n <- getLine
  echo True (read n)

echo :: Bool -> Int -> IO ()
echo _ 0 = return ()
echo True n = getLine >>= putStrLn >> echo False (n - 1)
echo False n = getLine >> echo True (n - 1)
