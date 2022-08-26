module Cap4 where
{- 1.Using library functions, define a function 
     halve :: [a] -> ([a],[a]) that splits an evenlengthed 
     list into two halves. For example: > halve [1,2,3,4,5,6]
     ([1,2,3],[4,5,6])
-}
 halve :: [a] -> ([a],[a])
 halve xs =(take (length xs `div` 2) xs, 
            drop (length xs `div` 2) xs)

{- 2.Define a function third :: [a] -> a that returns the third 
     element in a list that contains at least this many elements 
     using:
     a. head and tail;
     b. list indexing !!;
     c. pattern matching.
-}
 thirda :: [a] -> a
 thirda xs = head (tail (tail  xs))

 thirdb :: [a] -> a
 thirdb xs = xs !! 2

 thirdc :: [a] -> a
 thirdc (_:_:x:xs) = x

{- 3.Consider a function safetail :: [a] -> [a] that behaves in
     the same way as tail except that it maps the empty list to 
     itself rather than producing an error. Using tail and the 
     function null ::[a] -> Bool that decides if a list is empty 
     or not, define safetail using:
     a. a conditional expression;
     b. guarded equations;
     c. pattern matching.
-}
 safetaila :: [a] -> [a]
 safetaila xs = if null xs then [] else tail xs

 --safetailb :: [a] -> [a]
 safetailb xs | null xs   = []
              | otherwise = tail xs

 safetailc :: [a] -> [a]
 safetailc [] = []
 safetailc (x:xs) = xs

{- 4.In a similar way to && in section 4.4, show how the 
     disjunction operator || can be defined in four different 
     ways using pattern 
     matching.

 (||||) :: Bool -> Bool -> Bool
 True  |||| True  = True
 True  |||| False = True
 False |||| True  = True
 False |||| False = False

 (|||||) :: Bool -> Bool -> Bool
 False ||||| False = False
 _     ||||| _     = True

 (||`) :: Bool -> Bool -> Bool
 False ||' b     = b
 True  ||' _     = True
 (|||') :: Bool -> Bool -> Bool
 b |||' c | b == c    = b
          | otherwise = True
-}

{- 5.Without using any other library functions or operators, show 
     how the meaning of the following pattern matching definition 
     for logical conjunction && can be formalised using 
     conditional expressions:
     True && True = True
     _    && _    = False
Hint: use two nested conditional expressions.
-}

      --sin respuesta

{- 6.Do the same for the following alternative definition, and 
     note the difference in the number of conditional expressions 
     that are required:
     True && b = b
     False && _ = False
-}
      
      --sin respuesta

{- 7.Show how the meaning of the following curried function 
     definition can be formalised in terms of lambda expressions:
     mult :: Int -> Int -> Int -> Int
     mult x y z = x*y*z

     mult :: Int -> (Int -> (Int -> Int))
     mult = \x -> (\y -> (\z -> x*y*z))

-}

{- 8.The Luhn algorithm is used to check bank card numbers for 
     simple errors such as mistyping a digit, and proceeds as 
     follows:
     1.consider each digit as a separate number;
     2.moving left, double every other number from the second
     last;
     3.subtract 9 from each number that is now greater than 9;
     4.add all the resulting numbers together;
     5.if the total is divisible by 10, the card number is valid.
     Define a function luhnDouble :: Int -> Int that doubles a 
     digit and subtracts 9 if the result is greater than 9. For 
     example:
     > luhnDouble 3
     6
     > luhnDouble 6
     3
     Using luhnDouble and the integer remainder function mod, 
     define a function luhn :: Int -> Int -> Int -> Int -> Bool 
     that decides if a four-digit bank card number is valid. For 
     example:
     > luhn 1 7 8 4
     True
     > luhn 4 7 8 3
     False
     In the exercises for chapter 7 we will consider a more 
     general version of this function that accepts card numbers 
     of any length.
-}
 luhnDouble :: Int -> Int
 luhnDouble x | 2*x <=9   = 2*x
              | otherwise = 2*x-9

 luhn :: Int -> Int -> Int -> Int -> Bool
 luhn a b c d | (f b + f c + f d) `mod` 10 == 0 = True 
              | otherwise                       = False 
               where f x = luhnDouble x