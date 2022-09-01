module Week40Exercise0 where

data Palindrome
  = EvenLen String
  | OddLen String Char
  deriving (Eq, Show)

lenEven = even . length

getFirstHalf = take . (`div` 2) =<< length

getMiddle :: String -> Char
getMiddle x = x !! max 0 (length x `div` 2)

palindrome :: String -> Maybe Palindrome
palindrome x
  | x == reverse x =
    case () of
      ()
        | lenEven x -> Just (EvenLen (getFirstHalf x))
        | otherwise -> Just (OddLen (getFirstHalf x) (getMiddle x))
  | otherwise = Nothing

toString :: Palindrome -> String
toString (EvenLen n) = n ++ reverse n
toString (OddLen n c) = n ++ [c] ++ reverse n
