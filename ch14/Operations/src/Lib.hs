module Lib where

half :: Fractional a => a -> a
half x = x / 2

halfIdentity :: Fractional a => a -> a
halfIdentity = (*2) . half

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs = snd $ foldr go (Nothing, True) xs
  where go _ status@(_, False) = status
        go y (Nothing, t) = (Just y, t)
        go y (Just x, _) = (Just y, x >= y)

plusAssociative :: (Num a, Eq a) => a -> a -> a -> Bool
plusAssociative x y z = x + (y + z) == (x + y) + z

plusCommutative :: (Num a, Eq a) => a -> a -> Bool
plusCommutative x y = x + y == y + x

multiplyAssociative :: (Num a, Eq a) => a -> a -> a -> Bool
multiplyAssociative x y z = x * (y * z) == (x * y) * z

multiplyCommutative :: (Num a, Eq a) => a -> a -> Bool
multiplyCommutative x y = x * y == y * x

quotRule :: (Integral a, Eq a) => a -> a -> Bool
quotRule x y = (quot x y) * y + (rem x y) == x

divModRule :: (Integral a, Eq a) => a -> a -> Bool
divModRule x y = (div x y) * y + (mod x y) == x

powerCommutative :: (Integral a, Eq a) => a -> a -> Bool
powerCommutative x y = x ^ y == y ^ x

powerAssociative :: (Integral a, Eq a) => a -> a -> a -> Bool
powerAssociative x y z = x ^ (y ^ z) == (x ^ y) ^ z

reverseRule :: (Eq a) => [a] -> Bool
reverseRule xs = (reverse . reverse) xs == xs

functionApplication :: (Eq b) => (a -> b) -> a -> Bool
functionApplication f a = (f $ a) == f a

functionComposition :: (Eq c) => (b -> c) -> (a -> b) -> a -> Bool
functionComposition f g x = (f . g) x == lambda x
  where lambda = \z -> f (g z)

consConcatRule :: (Eq a) => [a] -> [a] -> Bool
consConcatRule xs ys = foldr (:) xs ys == flip(++) xs ys

concatRule :: (Eq a) => [[a]]  -> Bool
concatRule xs = foldr (++) [] xs == concat xs

takeLengthRule :: Int -> [a] -> Bool
takeLengthRule n xs = length (take n xs) == n

isomorphismRule :: (Read a, Show a, Eq a) => a -> Bool
isomorphismRule x = (read (show x)) == x

square :: (Num a) => a -> a
square x = x * x

squareIdentity :: Double -> Double
squareIdentity = square . sqrt