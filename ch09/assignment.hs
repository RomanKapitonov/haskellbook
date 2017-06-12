import Data.Char
import Data.Bool

-- 1. Query the types of isUpper and toUpper
-- isUpper :: Char -> Bool
-- toUpper :: Char -> Char

-- 2. Given the following behaviours, which would we use
-- to write a function that filters all the uppercase letters out
-- of a string String? Write that function such that, given the
-- input "HbEfLrLxO", your function returns "HELLO".

-- Prelude Data.Char> isUpper 'J'
-- True
-- Prelude Data.Char> toUpper 'j'
-- 'J'

filterUpper :: String -> String
filterUpper = filter isUpper

-- 3. Write a function that will capitalize the first letter of a String
-- and return the entire String. For example, if given the argument
-- "jullie", it will return "Julie"

upperFirst :: String -> String
upperFirst (x:xs) = toUpper x : xs

-- 4. Now make a new version of that function that is recursive
-- such that if you give it the input "woot" it will holler back
-- at you "WOOT". The type signature won't change, but you will
-- want to add a base case.

upperAll :: String -> String
upperAll [] = []
upperAll (x:xs) = toUpper x : upperAll xs

-- 5. To do the final excercise in this section, we'll need another standard
-- function for lists called head. Query the type of head and
-- experiment with it to see what it does. Now write a function
-- that will capitalize the first letter of a String and return
-- only that letter as the result

upperFirst' :: String -> Char
upperFirst' = toUpper . head

-- 6. Cool. Good work. Now rewrite it as a composed function. Then,
-- for fun, rewrite it pointfree
-- Already pointfree in 5

-- Writing your own standard functions

-- 1. myOr :: [Bool] -> Bool

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = x || myOr xs

-- 2. myAnu returns True if a -> Bool applied to any of the values
-- in the list returns true

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f xs = myOr (map f xs)

myAny' :: (a -> Bool) -> [a] -> Bool
myAny' _ [] = False
myAny' f (x:xs) = f x || myAny' f xs

-- 3. After you write the recursive myElem, write another verions that users
-- any
myElem :: Eq a => a -> [a] -> Bool
myElem e [] = False
myElem e (x:xs) = (e == x) || myElem e xs

myElem' :: Eq a => a -> [a] -> Bool
myElem' e [] = False
myElem' e xs = myAny (==e) xs

-- Prelude> myElem 1 [1..10]
-- True
-- Prelude> myElem 1 [2..10]
-- False

-- 4. Implement myReverse
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

-- 5. squish flattens a list of lists into a list
squish :: [[a]] -> [a]
squish [] = []
squish (xs:xss) = xs ++ squish xss

-- 6. squishMap maps a function over a list and concatenates the results.

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (x:xs) = f x ++ squishMap f xs

-- 7. squishAgain flattens a list of lists into a list. This time
-- re-use the squishMap function.

squishAgain :: [[a]] -> [a]
squishAgain [] = []
squishAgain xss = squishMap id xss

-- 8. myMaximum takes a comparison function and a list and returns
-- the greatest element of the list based on the last value that the
-- comparison returned GT for.

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x:[]) = x
myMaximumBy f (x:y:xs) =
  if (f x y) == GT
    then x
    else (myMaximumBy f (y:xs))

-- 9. myMinimumBy takes a comparison function and a list and returns
-- the least element of the list based on the last value that the 
-- comparison returned LT for

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (x:[]) = x
myMinimumBy f (x:y:xs) =
  if (f x y) == LT
    then x
    else (myMinimumBy f (y:xs))

-- Using the myMaximumBy and myMaximumBy finctions, write your own
-- versions of maximum and minimum.

myMaximum :: (Ord a) => [a] -> a
myMaximum xs = myMaximumBy compare xs

myMinimum :: (Ord a) => [a] -> a
myMinimum xs = myMinimumBy compare xs