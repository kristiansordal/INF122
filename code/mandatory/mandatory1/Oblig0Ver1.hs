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

-- get inputs with getcontents as it is lazy
-- somehow read the input line by line by passing it to a function
-- do every calculation in this function recursively

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

isStep :: [String] -> IO String
isStep (x : xs) = do
  let firstTriplet = read $ head (x : xs) :: (Double, Double, Double)
  let secondTriplet = read $ head $ tail (x : xs) :: (Double, Double, Double)
  let val1 = (\(a, b, c) -> a + b + c) firstTriplet
  let val2 = (\(a, b, c) -> a + b + c) secondTriplet
  let list = [val1, val2]
  let processedData =
        applyFilter (hpf highPassCutoff) $
          applyFilter (lpf lowPassCutoff) $
            reverse list
  let dataLength = length list
  let stepCount =
        (`div` 2) $
          zeroCrossings $
            reverse $
              take dataLength processedData
  if stepCount >= 1
    then do
      putStrLn "Step!"
      hFlush stdout
      isStep $ drop 1 (x : xs)
    else isStep $ drop 1 (x : xs)
isStep _ = do
  isStep ["1,1,1"]

-- pattern match for when list is empty, size 1, or size 2
-- make function recursive so that we can have an accumulator (the list of inputs)
-- recursiveStep :: [(Double, Double, Double)] -> IO String
-- recursiveStep [] = do
--   input <- getLine
--   let newInput = read input :: [(Double, Double, Double)]
--   recursiveStep newInput
-- recursiveStep [x] = do
--   input <- getLine
--   let newInput' = read input :: (Double, Double, Double)
--   let newList = append newInput' [x]
--   putStrLn "in [x]"
--   hFlush stdout
--   print $ show newList
--   recursiveStep newList
-- recursiveStep (x : y : ys) = do
--   -- input <- getLine
--   -- let newInput'' = read input :: (Double, Double, Double)
--   -- let newList = append newInput'' (x : y : ys)
--   let summedData = map (\(a, b, c) -> a + b + c) (x : y : ys)
--   let dataLength = length summedData
--   let processedData =
--         applyFilter (hpf highPassCutoff) $
--           applyFilter (lpf lowPassCutoff) $
--             summedData
--   let firstVal = head processedData
--   print firstVal
--   let secondVal = head (tail processedData)
--   print secondVal
--   if firstVal < 0 && secondVal > 0 || firstVal > 0 && secondVal < 0
--     then do
--       putStrLn "Step!"
--       hFlush stdout
--       recursiveStep (y : ys)
--     else recursiveStep (y : ys)

-- -- recursiveStep (x : y : ys) = do

-- append :: (Double, Double, Double) -> [(Double, Double, Double)] -> [(Double, Double, Double)]
-- append a [] = [a]
-- append a xs = foldr (:) [a] xs

-- -- append a (x:xs) = x : append a xs
