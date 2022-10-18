data Expr
  = Add Expr Expr
  | Mul Expr Expr
  | Lit Int
  | Var String

type Token = String

type Parser a = [Token -> Maybe (a, [Token])]

parseString :: String -> Parser String
parseString s = \ts -> case ts of
  (x : xs) | x == s -> Just (s, xs)
  _ -> Nothing

parseNumber :: Parser Int
parseNumber [] = Nothing
parseNumber (x : xs) = case readMaybe x of
  Just n -> Just (n, xs)
  Nothing -> Nothing

tokenize :: String -> [Token]
tokenize = words
