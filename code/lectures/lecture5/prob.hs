import Data.Char

strToUpper :: String -> String
-- strToUpper str = [toUpper (str !! i) | i <- [0 .. length str - 1]]
-- strToUpper str = [toUpper c | c <- str]
strToUpper = map toUpper
