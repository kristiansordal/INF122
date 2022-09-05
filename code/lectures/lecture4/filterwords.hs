module Filterwords where

removee :: String -> String
removee text = unwords (filter (notElem 'e') (words text))
