ztn :: [Integer] -> Maybe [Integer]
ztn x =
  if all (== 0) x
    then Nothing
    else Just x
