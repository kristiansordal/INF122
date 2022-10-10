module Oblig0Ver1_2 where

import Oblig0Common (highPassCutoff, hpf, lowPassCutoff, lpf)
import System.IO

mainAux :: [(Double, Double, Double)]
mainAux prevInput prevLpf prevHpf = do
  input <- getLine
  let (x, y, z) = read input :: (Double, Double, Double)
  let p = x + y + z
