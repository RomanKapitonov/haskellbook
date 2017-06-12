import Data.Bool

myHead :: [a] -> Maybe a
myHead [] = Nothing
myHead (x:_) = Just x

eftBool :: Bool -> Bool -> [Bool]
eftBool False False = [False]
eftBool True True = [True]
eftBool False True = [False, True]
eftBool True False = []

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd f t
  | f <= t    = f:eftOrd (succ f) t
  | otherwise = []

eftInt :: Int -> Int -> [Int]
eftInt f t
  | f <= t    = f:eftInt (succ f) t
  | otherwise = []

eftChar :: Char -> Char -> [Char]
eftChar f t
  | f <= t    = f:eftChar (succ f) t
  | otherwise = []

test = do
  (print . eftBool False) False
  (print . eftBool False) True

  (print . eftOrd LT) GT
  (print . eftOrd GT) LT

  (print . eftInt 1) 5
  print [1..5]

  (print . eftChar 'a') 'e'
  print ['a'..'e']

-- 1. Using takeWhile and dropWhile, write a function that takes a string and returns a list of strings,
-- using spaces to separate the elements of the string into words, as in the following sample:

-- *Main> myWords "all i wanna do is have some fun"
-- ["all","i","wanna","do","is","have","some","fun"]

myWords :: String -> [String]
myWords [] = []
myWords (' ':xs) = myWords xs
myWords xs = (takeWhile (/= ' ') xs):(myWords (dropWhile (/= ' ') xs))

-- 2 - PoemLines.hs

-- 3. Now let’s look at what those two functions have in common.
-- Try writing a new function that parameterizes the character
-- you’re breaking the string argument on and rewrite myWords and
-- myLines using it.

splitOnChar :: Char -> String -> [String]
splitOnChar _ [] = []
splitOnChar char all@(x:xs)
  | char == x = splitOnChar char xs
  | otherwise = (takeWhile condition all):(splitOnChar char (dropWhile condition all))
  where
    condition = (/= char)

mySqr = [x^2 | x <- [1..5]]
myCube = [y^3 | y <- [1..5]]

sqrCubeTuples = [(x, y) | x <- mySqr, y <- myCube, x < 50, y < 50]

-- Intermission: Exercises Will it blow up?
-- 1. Will the following expression return a value or be ⊥? 
-- [x^y | x <- [1..5], y <- [2, undefined]]
-- This will not work since we have to evaluate the values in the cons cells

-- 2. take 1 $ [x^y | x <- [1..5], y <- [2, undefined]]
-- This will work since we have to evaluate first level of spine and first cons cell only

-- 3. Will the following expression return a value?
-- sum [1, undefined, 3]
-- This will not work since we have to evaluate cons cells in order to get the result

-- 4. length [1, 2, undefined]
-- This will work since we have to only evaluate the spine

-- 5. length $ [1, 2, 3] ++ undefined
-- This will not work since two spines should be evaluated. It's fine for the first list, but the second one
-- contains a spine for undefined which will result in an error

-- 6. take 1 $ filter even [1, 2, 3, undefined]
-- This will work since evaluation will stop at the point where cons cell with undefined is not yet evaluated

-- 7. take 1 $ filter even [1, 3, undefined]
-- This will not work since we have to evaluate the values (cons cells) along with the spine to get rid of even entries

-- 8. take 1 $ filter odd [1, 3, undefined]
-- This will work since filter is applied to the first element which is required by take 1

-- 9. take 2 $ filter odd [1, 3, undefined]
-- This will work for the same reasons

-- 10. take 3 $ filter odd [1, 3, undefined]
-- This will not work since we have to evaluate the whole spine and all cons cells


-- 1. [1, 2, 3, 4, 5]
-- NF + WHNF

-- 2. 1 : 2 : 3 : 4 : _
-- WHNF

-- 3. enumFromTo 1 10
-- Neither

-- 4. length [1, 2, 3, 4, 5]
-- Neither

-- 5. sum (enumFromTo 1 10)
-- Neither

-- 6. ['a'..'m'] ++ ['n'..'z']
-- Neither

-- 7. (_, 'b')
-- WHNF

-- 1. Will the following expression return a value or be a bottom
-- take 1 $ map (+1) [undefined, 2, 3]
-- Fail as far as map gets applied to the first element in the list

-- 2. Will the following expression return a value
-- take 1 $ map (+1) [1, undefined, 3]
-- Ok

-- 3. take 2 $ map (+1) [1, undefined, 3]
-- Fail

-- 4.
-- itIsMystery :: String -> [Bool]
-- itIsMystery xs = map (\x -> elem x "aeiou") xs

-- 5.
-- map (^2) [1..10]
-- map minimum [[1..10], [10..20], [20..30]] -- n.b. `minimum` is not the same function -- as the `min` that we used before
-- map sum [[1..5], [1..5], [1..5]]

-- 6. map if-then-else
mmap :: a -> a -> [Bool] -> [a]
mmap t f = map (bool f t)

-- 1. Given the above, how might we write a filter function that
-- would give us all multiples of 3 out of a list from 1-30

filterMultsThree = filter (\x -> (x `mod` 3) == 0)

-- 2. Recalling what we learned about function composition, how could we compose the above
-- function with length function to tell us *how many* multiples of 3 are there between 1 and 30

lengthMultThree = length . filterMultsThree

-- 3. Next we are going to work on removing all articles ('the' 'a' and 'an') from sentences.
-- You want to get something that works like this:
-- Prelude> myFilter "the brown dog was a goof"
-- ["brown","dog","was","goof"]

myFilter :: String -> [String]
myFilter = (filter (\xs -> xs `notElem` ["the", "a", "an"])) . words


-- Zipping Exercises
-- 1. Write your own version of zip :: [a] -> [b] -> [(a, b)]
-- and ensure it behaves the same way as the original.

myZip :: [a] -> [b] -> [(a, b)]
myZip [] _ = []
myZip _ [] = []
myZip (x:xs) (y:ys) = (x, y) : myZip xs ys

-- 2. Do what you did for zip, but now for zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ [] _ = []
myZipWith _ _ [] = []
myZipWith f (x:xs) (y:ys) = f x y : myZipWith f xs ys

-- 3. Rewrite your zip in terms of the zipWith you wrote.
myZip' :: [a] -> [b] -> [(a, b)]
myZip' = myZipWith (,)
















