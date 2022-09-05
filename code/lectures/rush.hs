module Rush where

rushAlbumYears = filter (/= 1979) [1976 .. 1982]

rushAlbumTitles = ["2112", "A Farewell to Kings", "Hemispheres", "Permanent Waves", "Moving Pictures", "Signals"]

rushAlbums :: [(Integer, String)]
rushAlbums = zip rushAlbumYears rushAlbumTitles

displayAlbum :: String -> Integer -> String -> String
displayAlbum band year title = "In" ++ show year ++ band ++ " released" ++ title ++ "."

main = putStr $ unlines (map (uncurry (displayAlbum "Rush")) rushAlbums)
