module Exercises where

-- fold    :: (Monoid m, Foldable t) => t m -> m
-- foldMap :: (Monoid m, Foldable t) => (a -> m) -> t a -> m

sum :: (Foldable t, Num a) => t a -> a
sum = undefined

product :: (Foldable t, Num a) => t a -> a
product = undefined

elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem = undefined

minimum :: (Foldable t, Ord a) => t a -> Maybe a
minimum = undefined

maximum :: (Foldable t, Ord a) => t a -> Maybe a
maximum = undefined

null :: (Foldable t) => t a -> Bool
null = undefined

length :: (Foldable t) => t a -> Int
length = undefined

toList :: (Foldable t) => t a -> [a]
toList = undefined

fold :: (Foldable t, Monoid m) => t m -> m
fold = undefined

foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap = undefined
