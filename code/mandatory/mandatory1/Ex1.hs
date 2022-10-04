module Main where

import Graphics.Gloss
import Oblig0Common

main :: IO ()
main = do
    fileContents <- readFile "testData0"
    let s (x, y, z) = x + y + z
        summedData = (s . read) <$> lines fileContents

    let dataLength = length summedData
    let processedData =
          applyFilter (hpf highPassCutoff) $
            applyFilter (lpf lowPassCutoff) $
              reverse summedData

    display window white (picture summedData (take dataLength $ processedData))

picture :: [Float] -> [Float] -> Picture
picture d1 d2 = pictures [ makeLine d1
                         , color red $ makeLine d2
                         , line $ [(0, 0), (500, 0)]
                         ]
                             where
                                 makeLine :: [Float] -> Picture
                                 makeLine = line . zip [0, 0.04 ..]

window :: Display
window = InWindow "Nice Window" (768, 768) (10, 10)

