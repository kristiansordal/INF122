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
  isStep 0 []

-- all calculations are done in this method
isStep :: Integer -> [Double] -> IO ()
-- pattern match on the first two values, as we need as a minimum two values to
-- do any calculations
isStep 0 [] = do
  -- we use getline as it reads the file line by line, even in recursive calls
  line <- getLine

  -- process the line as a sum of the triplet
  let rawLine = read line :: (Double, Double, Double)
  let summedLine = (\(a, b, c) -> a + b + c) rawLine
  isStep 0 [summedLine]

-- same method for this match
isStep 0 [x] = do
  line <- getLine
  let rawLine = read line :: (Double, Double, Double)
  let summedLine = (\(a, b, c) -> a + b + c) rawLine
  -- pass the first two values as a list
  isStep 0 (summedLine : [x])

-- same method for getting and processing the next value
isStep x input = do
  line <- getLine
  let rawLine = read line :: (Double, Double, Double)
  let summedLine = (\(a, b, c) -> a + b + c) rawLine
  -- prepend the new value to the list
  let list = summedLine : input

  let dataLength = length list
  -- applies the filter to the list so far, it is important to use the entire list
  -- and not only the two values we care about as the lpf function takes the average of
  -- the entire list
  let processedData =
        applyFilter (hpf highPassCutoff) $
          applyFilter
            (lpf lowPassCutoff)
            list

  -- stepCount will either be 1 or 0, because we only look at two values, therefore we do not
  -- divide this value by two
  let stepCount =
        zeroCrossings $
          take 2 processedData

  if stepCount >= 1
    then do
      -- as we do not divide stepCount by two, we have to ensure that we only print step half the time
      -- this is because a step is defined by the period, which is defined by the crossing of the x-axis
      -- twice
      if even x
        then do
          putStrLn "Step!"
          hFlush stdout
          isStep (x + 1) list
        else do
          isStep (x + 1) list
    else do
      isStep x list
