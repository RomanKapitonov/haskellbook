module Answers where

awesome = ["Papuchon", "curry", ":)"]
alsoAwesome = ["Quake", "The Simons"]
allAwesome = [awesome, alsoAwesome]

-- 1. Given the definition of length above, what would the type signa- ture be?
-- How many arguments, of what type does it take? What is the type of the result it evaluates to?

-- length' :: [a] -> Integer

-- 2. What are the results of the following expressions? a) length [1, 2, 3, 4, 5]
-- b) length [(1, 2), (2, 3), (3, 4)]
-- 3
-- c) length allAwesome
-- 2
-- d) length (concat allAwesome)
-- 5

-- 3. Given what we know about numeric types and the type signature of length, look at these two expressions.
-- One works and one returns an error. Determine which will return an error and why.
-- (n.b., If you’re checking the type signature of length in GHC 7.10,
-- you will find Foldable t => t a representing [a], as with concat in the previous chapter.
-- Again, consider Foldable t to represent a list here, even though list is only one of the possible types.
-- We will explain it in detail later.)
--    Prelude> 6 / 3
-- :t (6 / 3) :: Fractional a => a
--    -- and
--    Prelude> 6 / length [1, 2, 3]
-- ERROR Can not match Int and Float

-- 4. How can you fix the broken code from the preceding exercise using a different division function/operator?
-- 6 `div` length [1, 2, 3]

-- 5. What is the type of the expression 2 + 3 == 5? What would we expect as a result?
-- :t (2 + 3 == 5) :: Bool

-- 6. What is the type and expected result value of the following:
--    Prelude> let x = 5
-- :t x :: Num t => t
--    Prelude> x + 3 == 5
-- :t x + 3 == 5 :: Bool

-- 7. Below are some bits of code. Which will work? Why or why not? If they will work,
-- what value would these reduce to?
--    Prelude> length allAwesome == 2 -- OK
--    Prelude> length [1, 'a', 3, 'b'] -- Error, type mismatch
--    Prelude> length allAwesome + length awesome -- OK
--    Prelude> (8 == 8) && ('b' < 'a') -- OK
--    Prelude> (8 == 8) && 9 -- Error, type mismatch

-- 8. Write a function that tells you whether or not a given String (or list) is a palindrome.
-- Here you’ll want to use a function called ’reverse,’ a predefined function that does just what it sounds like.
-- reverse :: [a] -> [a]
-- reverse "blah"
-- "halb"
-- isPalindrome :: (Eq a) => [a] -> Bool
-- isPalindrome x = undefined

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = x == reverse x

-- 9. Write a function to return the absolute value of a number using if-then-else
-- myAbs :: Integer -> Integer
-- myAbs = undefined

myAbs :: Integer -> Integer
myAbs n = if n < 0 then (-n) else n

-- 10. Fill in the definition of the following function, using fst and snd:
-- f :: (a, b) -> (c, d) -> ((b, d), (a, c))
-- f = undefined
f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f a b = ((snd a, snd b), (fst a, fst b))

-- Reading syntax
-- In the following examples, you’ll be shown syntactically incorrect code. Type it in and try to correct
-- it in your text editor, validating it with GHC or GHCi.
-- 1. Here, we want a function that adds 1 to the length of a string argument and returns that result.
-- x = (+)
-- F xs = w 'x' 1
--  where w = length xs

x = (+)
ff xs = w `x` 1
  where w = length xs

-- 2. This is supposed to be the identity function, id.
-- \X=x
id' :: a -> a
id' = \x -> x


-- 3. When fixed, this function will return 1 from the value [1, 2, 3].
-- Hint: you may need to refer back to the section about variables conventions in
-- “Hello Haskell” to refresh your memory of this notation.
-- \ x : xs -> x
f' :: [a] -> a
f' = \(x:xs) -> x

-- 4. When fixed, this function will return 1 from the value (1, 2)
-- f (a b) = A
f'' :: (a, b) -> a
f'' (a, b) = a

-- Match the function names to their types
-- 1. Which of the following types is the type of show?
-- a) show a => a -> String
-- b) Show a -> a -> String
-- c) Show a => a -> String - This one

-- 2. Which of the following types is the type of (==)?
-- a) a -> a -> Bool
-- b) Eq a => a -> a -> Bool - This one
-- c) Eq a -> a -> a -> Bool
-- d) Eq a => A -> Bool

-- 3. Which of the following types is the type of fst?
-- a) (a, b) -> a - This one
-- b) b -> a
-- c) (a, b) -> b

-- 4. Which of the following types is the type of (+)?
-- a) Num a -> a -> a -> Bool
-- b) Num a => a -> a -> Bool
-- c) num a => a -> a -> a
-- d) Num a => a -> a -> a - This one
-- e) a -> a -> a
