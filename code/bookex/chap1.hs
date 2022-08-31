module Chap1 where

-- Task 1
-- Give another possible calculation for the result of double (double 2).
dob :: Integer -> Integer
dob n = 2 * n

-- Task 2
-- Show that sum [x] = x for any number x.

-- Task 3
-- Define a function product that produces the product of al list of numbers,
-- and show using your definition that product [2,3,4] = 24.
prod [] = 1
prod (n : ns) = n * product ns
