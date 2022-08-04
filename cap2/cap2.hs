module Cap2 where

{--- exercise 1
	-- 2.2 Installing and starting
 2 + 3 * 4
 ( 2 + 3 ) * 4
 sqrt (3 ^ 2 + 4 ^ 2)
 	--2.3 Standard prelude
 head[1,2,3,4,5]
 tail[1,2,3,4,5]
 [1,2,3,4,5]!!2
 take 3 [1,2,3,4,5]
 drop 3 [1,2,3,4,5]
 length [1,2,3,4,5]
 sum [1,2,3,4,5]
 product [1,2,3,4,5]
 [1,2,3]++[4,5]
 reverse [1,2,3,4,5]
 	--2.4 Function application
 	--2.5 Haskell scripts
 	-}
 double x = x + x
 quadruple x = double (double x)
 -- Factorial of a positive integer :
 factorial n = product [1..n]
 -- Average of  a list of integers :
 average ns = div (sum ns) (length ns)
 a = b + c
  where
   b = 1
   c = 2
 d = a * 2
 
{-
-- 2. Parenthesise the following numeric expressions: --
	(2 ^ 3) * 4
	(2 * 3) + (4 * 5)
	2 + (3 * (4 ^ 5))
-}

{-
-- 3. The script below contains three syntactic errors. Correct these errors and then check that your
script works properly using GHCi. --
N = a ’div’ length xs
 where
 	``
   a = 10
  xs = [1,2,3,4,5]
-}
 n = div a (length xs)
  where
   a = 10
   xs = [1,2,3,4,5]

{-
-- 4.  The library function last selects the last element of a non-empty list; for example, last
[1,2,3,4,5] = 5. Show how the function last could be defined in terms of the other library
functions introduced in this chapter. Can you think of another possible definition?
-}
 last1 ns = head (reverse ns)

 {-
 --5. The library function init removes the last element from a non-empty list; for example, init
[1,2,3,4,5] = [1,2,3,4]. Show how init could similarly be defined in two different ways.
 -}
 init1 ns = reverse (tail (reverse ns))
 init2 ns = take (length (ns) -1 ) ns