module Cap6 where
{- 1.How does the recursive version of the factorial function
     behave if applied to a negative argument, such as (-1)? 
     Modify the definition to prohibit negative arguments by 
     adding a guard to the recursive case.
-}
 fac :: Int -> Int
 fac 0 = 1
 fac n | n > 0 = n * fac ( n-1 )
       | otherwise = error "no uses negativos"

{- 2.Define a recursive function sumdown :: Int -> Int that 
     returns the sum of the non-negative integers from a given 
     value down to zero. For example, sumdown 3 should return 
     the result 3+2+1+0 = 6.
-}
 sumdown :: Int -> Int
 sumdown 0 = 0
 sumdown x | x > 0  = x + sumdown (x-1)
           | otherwise = error "No uses negativos"

{- 3.Define the exponentiation operator ^ for non-negative 
     integers using the same pattern of recursion as the 
     multiplication operator *, and show how the expression 2 ^ 3 
     is evaluated using your definition.
     (^) :: Num a => a -> a -> a
     b ^ 0 = 1
     b ^ n | n > 0 = m * (m ^ (n-1))
     2^3 = 2 * (2 ^ 2)
         = 2 * (2 * (2 ^ 1))
         = 2 * (2 * (2 * (2 ^ 0)))
         = 2 * (2 * (2 * 1))
         = 2 * (2 * 2)
         = 2 * 4
         = 8
-}

{- 4.Define a recursive function euclid :: Int -> Int -> Int that 
     implements Euclid’s algorithm for calculating the greatest
     common divisor of two non-negative integers: if the two 
     numbers are equal, this number is the result; otherwise, the
     smaller number is subtracted from the larger, and the 
     process is then repeated. For example: 
     > euclid 6 27
     3
-}
 euclid :: Int -> Int -> Int
 euclid x y | x == y    = x
            | x < y     = euclid (y-x) x
            | otherwise = euclid (x-y) y

{- 5.Using the recursive definitions given in this chapter, show 
     how length [1,2,3], drop 3 [1,2,3,4,5], and init [1,2,3] are 
     evaluated.

     length [1,2,3] = 1 + length [2,3]
                    = 1 + (1 + length [3])
                    = 1 + (1 + (1 + length []))
                    = 1 + (1 + (1 + 0))
                    = 1 + (1 + 1)
                    = 1 + (2)
                    = 3

     drop 3 [1,2,3,4,5] = drop 2 [2,3,4,5]
                        = drop 1 [3,4,5]
                        = drop 0 [4,5]
                        = [4,5]
     
     init [1,2,3] = 1 : init [2,3]
                  = 1 : (2 : init [3])
                  = 1 : (2 : [])
                  = 1 : [2]
                  = [1,2]
-}

{- 6.Without looking at the definitions from the standard prelude,
     define the following library functions on lists using 
     recursion.
Note: most of these functions are defined in the prelude using 
other library functions rather than using explicit recursion, and
are generic functions rather than being specific to the type of
lists.
     a.Decide if all logical values in a list are True:
       and :: [Bool] -> Bool -}
 and1 :: [Bool] -> Bool
 and1 []        = True
 and1 [a]       = a
 and1 (False:_) = False
 and1 (True:as) = and as

{-   b.Concatenate a list of lists:
       concat :: [[a]] -> [a] -}
 concat1 :: [[a]] -> [a]
 concat1 (xs:[]) = xs
 concat1 (xs:xss) = xs ++ concat xss

{-   c.Produce a list with n identical elements:
       replicate :: Int -> a -> [a] -}
 replicate1 :: Int -> a -> [a]
 replicate1 0 a = []
 replicate1 n a | n > 0 = a : replicate1 (n-1) a

{-   d.Select the nth element of a list:
       (!!) :: [a] -> Int -> a
       []     !! n    = []
       (x:xs) !! 0    = x
       (x:xs) !! n    = xs !! (n-1)
-}
{-     e.Decide if a value is an element of a list: -}
 elem1 :: Eq a => a -> [a] -> Bool
 elem1 x []    = False
 elem1 x (y:ys) | x == y =True
                | otherwise = elem1 x ys 

{- 7.Define a recursive function 
     merge :: Ord a => [a] -> [a] -> [a] that merges two sorted
     lists to give a single sorted list. For example:
     > merge [2,5,6] [1,3,4]
     [1,2,3,4,5,6]
Note: your definition should not use other functions on sorted 
lists such as insert or isort, but should be defined using 
explicit recursion.
-}
 merge :: Ord a => [a] -> [a] -> [a]
 merge xs [] = xs
 merge [] ys = ys
 merge (x:xs) (y:ys) | x <= y    = x : merge xs (y:ys)
                     | otherwise = y : merge (x:xs) ys   

{- 8.Using merge, define a function msort :: Ord a => [a] -> [a] 
     that implements merge sort, in which the empty list and 
     singleton lists are already sorted, and any other list is
     sorted by merging together the two lists that result from 
     sorting the two halves of the list separately.
Hint: first define a function halve :: [a] -> ([a],[a]) that
splits a list into two halves whose lengths differ by at most one.
-}
 halve :: [a] -> ([a],[a])
 halve xs =(take (length xs `div` 2) xs, 
            drop (length xs `div` 2) xs)

 msort :: Ord a => [a] -> [a]
 msort []  = []
 msort [a] = [a]
 msort xs  = merge (msort (fst (halve xs)))
                   (msort (snd (halve xs)))

{- 9.Using the five-step process, construct the library functions
     that:
     a.calculate the sum of a list of numbers;
 suml :: [Int] -> Int

 suml []     =
 suml (n:ns) =

 suml []     = 1
 suma (n:ns) =
 
 suml []     = 1
 suml (n:ns) = n + suma ns
-}
 suml :: Num a => [a] -> a
 suml []     = 1
 suml (n:ns) = n + suml ns

{-   b.take a given number of elements from the start of a list;
 taken :: Int -> [a] -> [a]

 taken 0 []     = 
 taken 0 (x:xs) =
 taken n []     = 
 taken n (x:xs) =

 taken 0 []     = []
 taken 0 (x:xs) = []
 taken n []     = []
 taken n (x:xs) =
 
 taken 0 []     = []
 taken 0 (x:xs) = []
 taken n []     = []
 taken n (x:xs) = x:taken (n-1) xs
 -}
 taken :: Int -> [a] -> [a]
 taken 0 [_]    = []
 taken _ []     = []
 taken n (x:xs) = x:taken (n-1) xs

{-   c.select the last element of a non-empty list.
 lst :: [a] -> a

 lst [x]    = 
 lst (x:xs) =
 
 lst [x]    = x
 lst (x:xs) =
 
 lst [x]    = x
 lst (_:xs) = lst xs 
 -}
 lst :: [a] -> a
 lst [x]    = x
 lst (_:xs) = lst xs  