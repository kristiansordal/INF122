module Main where

import Oblig0Common
  ( applyFilter,
    highPassCutoff,
    hpf,
    lowPassCutoff,
    lpf,
    zeroCrossings,
  )
import System.IO

main = do
  -- Read user data
  input <- getContents
  -- Process data using filters
  let datapoints = map read (lines input) :: [(Double, Double, Double)]
  let summedData = map (\(a, b, c) -> a + b + c) datapoints
  let dataLength = length summedData
  let processedData =
        applyFilter (hpf highPassCutoff) $
          applyFilter (lpf lowPassCutoff) $
            reverse summedData
  main

-- pattern match for when list is empty, size 1, or size 2
-- make function recursive so that we can have an accumulator (the list of inputs)
-- isStep :: [(Double, Double, Double)] -> Bool
-- isStep (x : y : ys) = (length (a : as) /= 1) || (a > 0 && as < 0 || a < 0 && as > 0)
--   where
--     (a : as) = applyFilter (hpf highPassCutoff) $ applyFilter (lpf lowPassCutoff) $ reverse $ map (\(a, b, c) -> a + b + c) (x : y)
-- isStep [] = False
-- isStep [x] = False

printStep :: [Double] -> IO ()
printStep [] = printStep []
printStep [x] = printStep [x]
printStep (x : y : ys) = do
  input <- getLine
  let dataPoints = read input :: (Double, Double, Double)
  let summedData = (\(a, b, c) -> a + b + c) dataPoints
  let (a : b : bs) = append summedData (x : y : ys)
  if a > 0 && b < 0 || a < 0 && b > 0
    then do
      putStrLn "Step!"
      hFlush stdout
      printStep (b : bs)
    else printStep (b : bs)

append :: Double -> [Double] -> [Double]
append n xs = xs ++ [n]

-- stepRecursive :: [(Double, Double, Double)] -> IO String
-- stepRecursive (x : y : ys) = do
--   if isStep sum1 sum2
--     then putStrLn "Step!"
--     else stepRecursive (y : ys)
--   where
--     sum1 = applyFilter (hpf highPassCutoff) $ applyFilter (lpf lowPassCutoff) $ (\(a, b, c) -> a + b + c) $ x
--     sum2 = applyFilter (hpf highPassCutoff) $ applyFilter (lpf lowPassCutoff) $ (\(a, b, c) -> a + b + c) $ y

-- isStep :: Double -> Double -> Bool
-- isStep x y
--   | x > 0 && y < 0 = True
--   | x < 0 && y > 0 = True
--   | otherwise = False

-- one function which takes in a triplet list of doubles
-- then gets input, processes it
--
