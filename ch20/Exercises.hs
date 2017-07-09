module Exercises where

import Data.Monoid
import Data.Foldable

-- fold    :: (Monoid m, Foldable t) => t m -> m
-- foldMap :: (Monoid m, Foldable t) => (a -> m) -> t a -> m
-- foldMap f = foldr (mappend . f) mempty
-- foldr   :: Foldable t => (a -> b -> b) -> b -> t a -> b

sum' :: (Foldable t, Num a) => t a -> a
sum' = getSum . foldMap Sum

sum'' :: (Foldable t, Num a) => t a -> a
sum'' = foldr (+) 0

product' :: (Foldable t, Num a) => t a -> a
product' = getProduct . foldMap Product

product'' :: (Foldable t, Num a) => t a -> a
product'' = foldr (*) 1

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' x = getAny . foldMap (Any . (==x))

elem'' :: (Foldable t, Eq a) => a -> t a -> Bool
elem'' x = foldr ((||) . (==x)) False

data Min a = Min { getMin :: Maybe a } deriving (Eq, Show)

instance Ord a => Monoid (Min a) where
  mempty = Min Nothing
  Min Nothing `mappend` y = y
  x `mappend` Min Nothing = x
  Min x `mappend` Min y   = Min (min x y)

minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' = getMin . foldMap (Min . Just)

minimum'' :: (Foldable t, Ord a) => t a -> Maybe a
minimum'' = foldr f Nothing
  where f x Nothing  = Just x
        f x (Just y) = Just (min x y)

data Max a = Max { getMax :: Maybe a } deriving (Eq, Show)

instance Ord a => Monoid (Max a) where
  mempty = Max Nothing
  Max Nothing `mappend` y = y
  x `mappend` Max Nothing = x
  Max x `mappend` Max y   = Max (max x y)

maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' = getMax . foldMap (Max . Just)

maximum'' :: (Foldable t, Ord a) => t a -> Maybe a
maximum'' = foldr f Nothing
  where f x Nothing  = Just x
        f x (Just y) = Just (max x y)

null' :: (Foldable t) => t a -> Bool
null' = foldr (\_ _ -> False) True

length' :: (Foldable t) => t a -> Int
length' = foldr (\_ c -> c + 1) 0

toList' :: (Foldable t) => t a -> [a]
toList' = foldr (\x acc -> x:acc) []

toList'' :: (Foldable t) => t a -> [a]
toList'' = foldr (:) []

toList''' :: (Foldable t) => t a -> [a]
toList''' = foldMap (:[])

fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap id

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr (mappend . f) mempty
