module Qsorti where

qsort []     = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
               where
                smaller = [a | a <- xs, a <= x]
                larger  = [b | b <- xs, b > x]

qsorti []     = []
qsorti (x:xs) = qsorti larger ++ [x] ++ qsorti smaller
               where
                smaller = [a | a <- xs, a <= x]
                larger  = [b | b <- xs, b > x]