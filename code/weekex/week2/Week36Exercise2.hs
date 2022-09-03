module Week36Exercise2 where

halfPalindrome :: String -> Maybe String
-- Solution with if else statements.
-- halfPalindrome a =
--   if a == reverse a
--     then Just (take (length a `div` 2) a)
--     else Nothing

-- Solution with guards
halfPalindrome a
  | a == reverse a = Just (take (length a `div` 2) a)
  | otherwise = Nothing

decomposePalindrome :: String -> Maybe (String, Maybe Char)
-- Solution with if else statements
-- decomposePalindrome b =
--   if b == reverse b
--     then
--       if even (length b)
--         then Just (take (length b `div` 2) b, Nothing)
--         else -- Just (take (length b `div` 2) b, Just (head (drop (length b `div` 2) b)))
--           Just (take (length b `div` 2) b, Just (b !! (length b `div` 2)))
--     else Nothing

-- Solution with guards and case statements
decomposePalindrome b
  | b == reverse b =
    case () of
      ()
        | even (length b) -> Just (take (length b `div` 2) b, Nothing)
        | otherwise -> Just (take (length b `div` 2) b, Just (b !! (length b `div` 2)))
  | otherwise = Nothing

createPalindrome :: String -> Maybe Char -> String
-- Soltuion with pattern mathcing
createPalindrome c Nothing = c ++ reverse c
createPalindrome c (Just d) = c ++ [d] ++ reverse c
