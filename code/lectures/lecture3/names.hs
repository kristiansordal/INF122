module Names where

combineNames :: (String, String, Integer) -> String
combineNames (firstName, lastName, yearOfBirth) = firstName ++ " " ++ lastName
