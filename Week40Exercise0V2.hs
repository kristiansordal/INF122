data Palindrome
  = Palindromeodd String Char
  | Palindromeeven String
  deriving (Show)

palindrome :: String -> Maybe Palindrome
getMiddle :: String -> Char

getMiddle a = a !! (length a `div` 2) + 1

palindrome a =
  if isPalindrome a then
    (if length a `mod` 2 == 0 then Just (Palindromeeven (take length a `div` 2))


isPalindrome :: String -> Bool
isPalindrome word = reverse word == word

