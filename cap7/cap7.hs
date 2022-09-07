module Cap7 where
 import Data.Char
 type Bit = Int
{- 1.Show how the list comprehension [f x | x <- xs, p x] can be 
     re-expressed using the higher-order functions map and filter.
     
     map f (filter p xs) -}

{- 2.Without looking at the definitions from the standard prelude, define the 
     following higher-order library functions on lists.
Note: in the prelude the first two of these functions are generic functions 
rather than being specific to the type of lists.
     a.Decide if all elements of a list satisfy a predicate:-}
 all1 :: (a -> Bool) -> [a] -> Bool 
 all1 p xs = and (map p xs )

{-   b.Decide if any element of a list satisfies a predicate:
       any :: (a -> Bool) -> [Bool] -> Bool -}
 any1 :: (a -> Bool) -> [a] -> Bool
 any1 p xs = or (map p xs)

{-   c.Select elements from a list while they satisfy a predicate:
       takeWhile :: (a -> Bool) -> [a] -> [a] -}
 takeWhile1 :: (a -> Bool) -> [a] -> [a]
 takeWhile1 p []     = []
 takeWhile1 p (x:xs) | p x       = x : takeWhile1 p xs
                     | otherwise = []

{-   d.Remove elements from a list while they satisfy a predicate:
       dropWhile :: (a -> Bool) -> [a] -> [a] -}
 dropWhile1 :: (a -> Bool) -> [a] -> [a]
 dropWhile1 p []     = []
 dropWhile1 p (x:xs) | p x       = dropWhile1 p  xs 
                     | otherwise = x:xs

{- 3.Redefine the functions map f and filter p using foldr.
 foldr :: (a -> b -> b) -> b -> [a] -> b
 foldr f v [] = v
 foldr f v (x:xs) = f x (foldr f v xs)
 foldr (#) v [x0,x1,...,xn] = x0 # (x1 # (... (xn # v) ...))
-}
 mapf :: (a -> b) -> [a] -> [b]
 mapf f = foldr (\x xs -> f x : xs) []  
 filterf :: (a -> Bool) -> [a] -> [a]
 filterf p = foldr (\x xs -> if p x then x:xs else xs) []

{- 4.Using foldl, define a function dec2int :: [Int] -> Int that converts a 
     decimal number into an integer. For example:
     > dec2int [2,3,4,5]
     2345
 foldl :: (a -> b -> a) -> a -> [b] -> a
 foldl f v [] = v
 foldl f v (x:xs) = foldl f (f v x) xs
 foldl (#) v [x0,x1,...,xn] = (... ((v # x0) # x1) ...) # xn
-}
 dec2int :: [Int] -> Int
 dec2int = foldl (\a b -> a*10 + b ) 0

{- 5.Without looking at the definitions from the standard prelude, define the 
     higher-order library function curry that converts a  function on pairs 
     into a curried function, and, conversely, the function uncurry that 
     converts a curried function with two arguments into a function on pairs.
Hint: first write down the types of the two functions.
-}
 curry1 :: ((a,b) -> c) -> (a -> b -> c)
 curry1 f = \x y -> f (x,y)
 uncurry1 :: (a -> (b -> c)) -> ((a,b) -> c)
 uncurry1 f = \(x,y) -> f x y 

{- 6.A higher-order function unfold that encapsulates a simple pattern of 
     recursion for producing a list can be defined as follows:
     unfold p h t x | p x       = []
                    | otherwise = h x : unfold p h t (t x)
     That is, the function unfold p h t produces the empty list if the predicate 
     p is true of the argument value, and otherwise produces a non-empty list by 
     applying the function h to this value to the head, and the function t to 
     generate another argument that is recursively processed in the way to 
     produce the tail of the list. For example, the function int2bin can be 
     rewritten more compactly using unfold as follows:
     int2bin = unfold (== 0) (‘mod‘ 2) (‘div‘ 2)
     Redefine the functions chop8, map f and iterate f using unfold.
-}
 unfold p h t x | p x       = []
                | otherwise = h x : unfold p h t (t x)
 int2bin :: Int -> [Bit]
 int2bin = unfold (== 0) (`mod` 2) (`div` 2)
 chop8u :: [Bit] -> [[Bit]]
 chop8u = unfold (== []) (take 8) ((drop 8))
 mapu :: Eq a => (a -> b) -> [a] -> [b]
 mapu f = unfold (== []) (f.head) (tail)
 iterateu :: Eq a => Num a => (a -> a) -> a -> [a]
 iterateu f = unfold (==0) (id) (f) 

{- 7.Modify the binary string transmitter example to detect simple transmission 
     errors using the concept of parity bits. That is, each eight-bit binary 
     number produced during encoding is extended with a parity bit, set to one if
     the number contains an odd number of ones, and to zero otherwise. In turn,
     resulting nine-bit binary number consumed during decoding is checked to
     ensure that its parity is correct, with the parity bit being discarded if 
     this is the case, and a parity error being reported otherwise.
Hint: the library function error :: String -> a displays the given string as an
error message and terminates the program; the polymorphic result type ensures 
that error can be used in any context.
-}
 bin2int :: [Bit] -> Int
 bin2int = foldr (\x y -> x + 2*y) 0
 make8 :: [Bit] -> [Bit]
 make8 bits = take 8 (bits ++ repeat 0)
 make9 :: [Bit] -> [Bit]
 make9 bits | odd(sum (bits)) = bits ++ int2bin 1
            | otherwise       = take 9 (bits ++ repeat 0)
 encode :: String -> [Bit]
 encode = concat . map (make9 . make8 . int2bin . ord)

 chop9 :: [Bit] -> [[Bit]]
 chop9 [] = []
 chop9 bits = take 9 bits : chop9 (drop 9 bits)
 chop8 :: [Bit] -> [Bit]
 chop8 [] = []
 chop8 bits | (sum (take 8 bits)) `mod` 2 == sum (drop 8 bits) = take 8 bits
            | otherwise                               = error "error de paridad"
 decode :: [Bit] -> String
 decode = map (chr . bin2int) . map (chop8). chop9
 transmit :: String -> String
 transmit = decode . channel . encode
 channel :: [Bit] -> [Bit]
 channel = id
 
{- 8.Test your new string transmitter program from the previous 
     exercise using a faulty communication channel that forgets 
     the first bit, which can be modelled using the tail function 
     on lists of bits.
-}
 channelt :: [Bit] -> [Bit]
 channelt = tail
{- 9.Define a function 
     altMap :: (a -> b) -> (a -> b) -> [a] -> [b] 
     that alternately applies its two argument functions to 
     successive elements in a list, in turn about order. For 
     example: 
     > altMap (+10) (+100) [0,1,2,3,4]
     [10,101,12,103,14]
-}
 altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
 altMap h t [] = []
 altMap h t (x:xs) = h x : altMap t h xs 

{- 10.Using altMap, define a function luhn :: [Int] -> Bool that 
      implements the Luhn algorithm from the exercises in chapter 
      4 for bank card numbers of any length. Test your new 
      function using your own bank card.
-}
 luhnDouble :: Int -> Int
 luhnDouble x | 2*x <=9   = 2*x
              | otherwise = 2*x-9
 suml :: [Int] -> [Int]
 suml = (altMap (id) (luhnDouble)).tail.(altMap (id) (luhnDouble))
 verifyl :: [Int] -> Bool
 verifyl xs = ((sum xs) `mod` 10) == 0
 luhn :: [Int] -> Bool
 luhn = verifyl.suml