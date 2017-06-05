{-# OPTIONS_GHC -Wall #-}

-- Intermission: Exercises
-- Below you’ll find a list of several standard functions we’ve talked about previously.
-- Under that is a list of their type signatures. Match the function to its type signature.
-- Try to do it without peeking at the type signatures (either in the text or in GHCi) and then check your work.
-- You may find it easier to start from the types and work out what you think a function of that type would do.
-- 1. Functions:
-- a) not
-- b) length
-- c) concat
-- d) head
-- e) (<)
-- 2. Type signatures:
-- a) head :: [a] -> a
-- b) concat :: [[a]] -> [a]
-- c) not :: Bool -> Bool
-- d) head :: [a] -> Int
-- e) (<) :: Ord a => a -> a -> Bool

-- Intermission: Exercises
-- Given a function and its type, tell us what type results from applying some or all of the arguments.
-- You can check your work in the REPL like this (using the first question as an example):
-- Prelude> let f :: a -> a -> a -> a; f = undefined
-- Prelude> let x :: Char; x = undefined
-- Prelude> :t f x

-- 1. Ifthetypeoffisa -> a -> a -> a,andthetypeof𝑥isChar then the type of f x is
-- a) Char -> Char -> Char - This one
-- b) x -> x -> x -> x
-- c) a -> a -> a
-- d) a -> a -> a -> Char

-- 2. If the type of g is a -> b -> c -> b
-- ,then the type of g 0 'c' "woot" is
-- let g :: a -> b -> c -> b; g = undefined
-- a) String
-- b) Char -> String
-- c) Int
-- d) Char - This one

-- 3. If the type of h is (Num a, Num b) => a -> b -> b
-- ,then the type of h 1.0 2 is
-- let h :: (Num a, Num b) => a -> b -> b; h = undefined
-- a) Double
-- b) Integer
-- c) Integral b => b
-- d) Num b => b - This one

-- 4. If the type of h is (Num a, Num b) => a -> b -> b
-- ,then the type of
-- h 1 (5.5 :: Double) is
-- let h :: (Num a, Num b) => a -> b -> b; h = undefined
-- a) Integer
-- b) Fractional b => b
-- c) Double - This one
-- d) Num b => b

-- 5. If the type of jackal is (Ord a, Eq b) => a -> b -> a
-- ,then the type of
-- let jackal :: (Ord a, Eq b) => a -> b -> a; jackal = undefined
-- jackal "keyboard" "has the word jackal in it"
-- a) [Char] -> [Char]
-- b) Eq b => b
-- c) b -> [Char] - This one
-- d) b
-- e) Eq b => b

-- 6. If the type of jackal is (Ord a, Eq b) => a -> b -> a
-- ,then the type of
-- jackal "keyboard"
-- a) b
-- b) Eq b => b
-- c) [Char]
-- d) b -> [Char]
-- e) Eq b => b -> [Char] - This one

-- 7. If the type of kessel is (Ord a, Num b) => a -> b -> a
-- ,then the type of
-- let kessel :: (Ord a, Num b) => a -> b -> a; kessel = undefined
-- kessel 1 2 is
-- a) Integer
-- b) Int
-- c) a
-- d) (Num a, Ord a) => a  - This one
-- e) Ord a => a
-- f) Num a => a

-- 8. If the type of kessel is (Ord a, Num b) => a -> b -> a
-- ,then the type of
-- kessel 1 (2 :: Integer) is
-- a) (Num a, Ord a) => a - This one
-- b) Int
-- c) a
-- d) Num a => a
-- e) Ord a => a
-- f) Integer

-- 9. If the type of kessel is (Ord a, Num b) => a -> b -> a
-- ,then the type of
-- kessel (1 :: Integer) 2 is
-- a) Num a => a
-- b) Ord a => a
-- c) Integer - This one
-- d) (Num a, Ord a) => a
-- e) a

-- functionH :: [a] -> a
-- functionH (x:_) = x

-- functionC :: Ord a => a -> a -> Bool
-- functionC x y = if (x > y) then True else False

-- functionS :: (a, b) -> b
-- functionS (x, y) = y

-- i :: a -> a
-- i x = x

-- c :: a -> b -> a
-- c x y = x

-- co :: (b -> c) -> (a -> b) -> (a -> c)
-- co f g x = f (g x)

-- a :: (a -> c) -> a -> a
-- a f x = x

-- a' :: (a -> b) -> a -> b
-- a' f x = f x

-- module Sing where

-- fstString :: [Char] -> [Char]
-- fstString x = x ++ " in the rain"

-- sndString :: [Char] -> [Char]
-- sndString x = x ++ " over the rainbow"
-- sing = if (x < y) then fstString x else sndString y
--   where x = "Singin"
--         y = "Somewhere"

-- module Arith3Broken where
-- main :: IO ()
-- main = do
--   print (1 + 2)
--   putStrLn $ show 10
--   print (negate (-1))
--   print ((+) 0 blah)
--     where blah = negate 1

-- f :: Int -> String
-- f = undefined

-- g :: String -> Char
-- g = undefined

-- h :: Int -> Char
-- h x = g (f x)

-- data A
-- data B
-- data C

-- q :: A -> B
-- q = undefined

-- w :: B -> C
-- w = undefined

-- e :: A -> C
-- e x = w (q x)

-- data X
-- data Y
-- data Z

-- xz :: X -> Z
-- xz = undefined

-- yz :: Y -> Z
-- yz = undefined

-- xform :: (X, Y) -> (Z, Z)
-- xform (x, y) = (xz x, yz y)

-- munge :: (x -> y) -> (y -> (w, z)) -> x -> w
-- munge f g x = fst (g (f x))

-- Write the Eq instance for the datatype provided.
-- 1. It’s not a typo, we’re just being cute with the name.
data TisAnInteger = TisAn Integer

instance Eq TisAnInteger where
  (TisAn a) == (TisAn b) =  a == b

-- 2. data TwoIntegers = Two Integer Integer
data TwoIntegers = Two Integer Integer

instance Eq TwoIntegers where
  (==) (Two a1 a2) (Two b1 b2) = (a1 == b1) && (a2 == b2)
-- 3. data StringOrInt = TisAnInt Int
--       | TisAString String
data StringOrInt = TisAnInt Int | TisAString String

instance Eq StringOrInt where
  (==) (TisAnInt a) (TisAnInt b) = a == b
  (==) (TisAString a) (TisAString b) = a == b
  (==) _ _ = False

-- 4. data Pair a =
-- Pair a a

data Pair a = Pair a a deriving Show

instance Eq a => Eq (Pair a) where
  (Pair a b) == (Pair x y) = a == x && b == y

-- 5. data Tuple a b =
-- Tuple a b

data Tuple a b = Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
  (Tuple a b) == (Tuple x y) = a == x && b == y

-- 6. data Which a = ThisOne a | ThatOne a

data Which a = ThisOne a | ThatOne a

instance Eq a => Eq (Which a) where
  (==) (ThisOne x) (ThisOne y) = x == y
  (==) (ThatOne x) (ThatOne y) = x == y
  (==) _ _ = False

-- 7. data EitherOr a b = Hello a
-- | Goodbye b

data EitherOr a b = Hello a | Goodbye b deriving Show

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (Hello x) == (Hello y) = x == y
  (Goodbye x) == (Goodbye y) = x == y
  (==) _ _ = False

testInstances :: IO ()
testInstances = do
  print $ (TisAn 5) == (TisAn 5)
  print $ (Two 5 5) == (Two 5 5)
  print $ (TisAnInt 5) == (TisAnInt 5)
  print $ (TisAString "5") == (TisAString "5")
  print $ (TisAString "5") == (TisAnInt 5)
  print $ (Pair 5 5) == (Pair 5 5)
  print $ (Tuple 5 "5") == (Tuple 5 "5")
  print $ (ThisOne 1) == (ThisOne 1)
  print $ (ThatOne 1) == (ThatOne 1)
  print $ (Hello "a" :: EitherOr String Int) == (Hello "a" :: EitherOr String Int)
  print $ (Goodbye 5 :: EitherOr String Int) == (Goodbye 5 :: EitherOr String Int)
  print $ (Goodbye 5 :: EitherOr String Int) == (Hello "a" :: EitherOr String Int)
