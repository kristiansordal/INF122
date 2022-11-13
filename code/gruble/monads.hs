-- yr.no/weatherdata.txt

import Text.Read

-- f :: String -> [Float]
-- fetchFile :: String -> String
-- parseFile :: String -> [WeatherData]
-- getTemps :: [WeatherData] -> [Float]
-- f = getTemps . parseFile . fetchFile
-- f webAddress = getTemps (parseFile (fetchfile webAdress))

-- f :: String -> Maybe Int
-- g :: Int -> Maybe Int
-- h :: Int -> Maybe Int
-- A -> m B
-- B -> m C
-- c -> M d

readInt :: String -> Maybe Int
readInt = readMaybe

divide100By :: Int -> Maybe Int
divide100By 0 = Nothing
divide100By a = Just (100 `div` a)

g :: String -> Maybe Int
g str = readInt str divide100By

-- g str = do
--   parsedInt <- readInt str
--   result <- divide100By parsedInt
--   return result

-- f :: String -> Maybe Int
-- f str =
--   let maybeInt = readInt str
--    in case maybeInt of
--         Just i -> divide100By i
--         Nothing -> Nothing

type State = Int

map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x : xs) = [f x | x <- xs]

-- map f (x : xs) = f x : map f xs

mapState :: (a State -> (b, State)) -> [a] -> State -> [b]
mapState f [] state = []
mapState f (x : xs) state =
  let (value, newState) = f x state
   in value : mapState f xs newState

f :: Maybe Int -> State -> (Int, State)
f Nothing counter = (counter, counter + 1)
f (Just x) counter = (x, counter)

replaceNothing :: [Maybe Int] -> [Int]
replaceNothing xs = map f xs
  where
    f Nothing = 0
    f (Just x) = x
