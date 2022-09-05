singleNames :: [String]
singleNames = ["Ane", "Rikke", "Marie", "Gunn"]

-- doubleNames :: [String] -> [String]
-- doubleNames = [first ++ "-" ++ last | first <- singleNames, last <- singleNames, if first /= last]

-- doubleNames = do
--   first <- singleNames
--   last <- singleNames $
--   guard (first /= last)
--   return $ first ++ "-" ++ last
