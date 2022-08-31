module Week36Exercise2 where

import Data.Char

halfPalindrome :: String -> Maybe String
halfPalindrome a =
  if a == reverse a
    then Just (take (length a `div` 2) a)
    else Nothing

decomposePalindrome :: String -> Maybe (String, Maybe Char)
decomposePalindrome b =
  if b == reverse b
    then
      if even (length b)
        then Just (take (length b `div` 2) b, Nothing)
        else -- Just (take (length b `div` 2) b, Just (head (drop (length b `div` 2) b)))
          Just (take (length b `div` 2) b, Just (b !! max 0 (length b `div` 2)))
    else Nothing

createPalindrome :: String -> Maybe Char -> String
createPalindrome c Nothing = c ++ reverse c
createPalindrome c (Just d) = c ++ [d] ++ reverse c
