module Cap3 where
{--- 1. What are the types of the following values?
	[’a’,’b’,’c’] --> [Char]
	(’a’,’b’,’c’) --> (Char, Char, Char)
	[(False,’O’),(True,’1’)] --> [(Bool,Char)]
	([False,True],[’0’,’1’]) --> ([Bool],[Char])
	[tail, init, reverse] --> [[a]->[a]]
-}
{--- 2. Write down definitions that have the following types; it does not matter what the 
		definitions actually do as long as they are type correct.
-}		
 bools :: [Bool]
 bools = [True, False, True]
 nums :: [[Int]]
 nums = [[1],[1,2],[1,2,3],[1,2,3,4]]
 add :: Int -> Int -> Int -> Int
 add x y z = x^2 + y^2 + z^2
 copy :: a -> (a,a)
 copy x = (x,x)
 apply :: (a -> b) -> a -> b
 apply f x = f x
{--- 3. What are the types of the following functions?
-}
 second :: [a] -> a
 second xs = head (tail xs)
 swap :: (a,b) -> (b,a) 
 swap (x,y) = (y,x)
 pair  :: a -> b -> (a,b)
 pair x y = (x,y)
 double :: Num a => a -> a
 double x = x*2
 palindrome :: Eq a => [a] ->  Bool
 palindrome xs = reverse xs == xs 
 twice :: (b -> b) -> b -> b
 twice f x = f (f x) 

{---
	Hint: take care to include the necessary class constraints in the types if the functions 
	are defined using overloaded operators.
-}
{--- 4. Check your answers to the preceding three questions using GHCi
-}
{--- 5. Why is it not feasible in general for function types to be instances of the Eq class? 
		When is it feasible? Hint: two functions of the same type are equal if they always 
		return equal results for equal arguments.
-}