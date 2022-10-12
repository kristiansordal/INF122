module Week42Exercise0 where

applyFunctions :: [a -> b] -> [a] -> [b]
applyFunctions = zipWith ($)
