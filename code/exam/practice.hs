import Data.Map (Map)
import Data.Map qualified as Map

main = do
  name <- parse <$> getLine
  print name

parse :: String -> String
parse name = last (words name) ++ ", " ++ head (words name)

isConsonant :: Char -> Bool
isConsonant = (`elem` "bcdfghjklmnpqrstvwxz")

translate :: String -> String
translate = concatMap (\x -> if isConsonant x then [x] ++ "o" ++ [x] else [x])

translate' :: String -> String
translate' [] = ""
translate' (x : xs) =
  if isConsonant x
    then do
      s <- [x] ++ "o" ++ [x]
      return s ++ translate' xs
    else translate' xs

everyOther :: [a] -> [a]
everyOther [] = []
everyOther [x] = [x]
everyOther (x : y : ys) = x : everyOther ys

differences :: [Integer] -> [Integer]
differences list = [x - y | x <- list, y <- list, x - y > 0]

data N = A | B | C
  deriving (Show, Eq, Ord)

type Graph label node = Map node (Map label node)

graph0 :: Graph Char N
graph0 =
  Map.fromList
    [ (A, Map.fromList [('r', B)]),
      (B, Map.fromList [('o', B), ('t', C)]),
      (C, Map.fromList [('e', A), ('t', C)])
    ]

insertLabeledEdge :: (Ord node) => Graph label node -> node -> node -> label -> Graph label node
insertLabeledEdge g n1 n2 l = Map.insert n1 (Map.singleton l n1) g

goNext :: (Ord node, Ord label) => Graph label node -> node -> label -> Maybe node
goNext graph start label = do
  labelMap <- Map.lookup start graph
  Map.lookup label labelMap

goNext' :: (Ord node, Ord label) => Graph label node -> node -> label -> Maybe node
goNext' graph start label = Map.lookup start graph >>= Map.lookup label

followPath :: (Ord node, Ord label) => Graph label node -> node -> [label] -> Maybe node
followPath g n [] = Just n
followPath g n (x : xs) =
  case goNext' g n x of
    (Just node) -> followPath g node xs
    Nothing -> Just n
