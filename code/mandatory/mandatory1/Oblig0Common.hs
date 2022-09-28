module Oblig0Common where

-- Compute the average of a list of values
average :: (Fractional a) => [a] -> a
average [] = 0
average [x] = x
average xs = sum xs / fromIntegral (length xs)

-- A simple lowpass filter with adjustable cut - off
lpf :: (Fractional a) => Integer -> [a] -> a
lpf n [] = 0
lpf n xs = average $ take (fromIntegral n) xs

-- A simple high pass filter with adjustable cut-off
hpf :: (Floating a) => Integer -> [a] -> a
hpf n [] = 0
hpf n (x : xs) = x - lpf n (x : xs)

-- Extend a finite signal with an infinite constant past
extend :: Num a => [a] -> [a]
extend [] = 0 : repeat 0
extend [x] = repeat x
extend (x : xs) = x : extend xs

-- Apply a filter to a list of values
applyFilter :: (Num a, Floating a) => ([a] -> a) -> [a] -> [a]
applyFilter fil = map fil . iterate tail . extend

-- Count the number of zero-crossings in a signal represented by a list
zeroCrossings :: (Num a, Ord a) => [a] -> Integer
zeroCrossings [] = 0
zeroCrossings [x] = 0
zeroCrossings (x : y : ys) =
  if x > 0 && y < 0 || x < 0 && y > 0
    then 1 + zeroCrossings (y : ys)
    else zeroCrossings (y : ys)

lowPassCutoff :: Integer
-- lowPassCutoff = 188

lowPassCutoff = 33

highPassCutoff :: Integer
-- highPassCutoff = 27

highPassCutoff = 9
