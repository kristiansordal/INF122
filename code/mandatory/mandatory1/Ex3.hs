module Main where

import Graphics.Gloss

main :: IO ()
main = animate window white drawFrame

band = 0.3

drawFrame :: Float -> Picture
drawFrame time = pictures $ map createSquare [(x, y) | x <- [-60..60], y <- [-60..60] ]
    where createSquare (x, y) = move x y $ color (getColor x y) $ rectangleSolid 6 6
          move x y = translate (x * 6) (y * 6)
          getColor x y = let foo = abs (sin ((x/25) + time*2) - (y / 50))
                          in if foo <= band
                                 then makeColor ((1.0 - (foo * recip band)) ^ 2 * 0.7) 0.3 0.3 1.0
                                 else greyN $ 0.3

window :: Display
window = InWindow "Nice Window" (768, 768) (10, 10)

