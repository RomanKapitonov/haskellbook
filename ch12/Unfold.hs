module Unfold where

import Data.List

myIterate :: (a -> a) -> a -> [a]
myIterate f start = (start) : (myIterate f (f start))

-- take 10 $ unfoldr (\b -> Just (b, b + 1)) 0
-- [0,1,2,3,4,5,6,7,8,9]

-- take 10 $ myUnfoldr (\b -> Just (b, b + 1)) 0
-- [0,1,2,3,4,5,6,7,8,9]
myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f start = case f start of
  Just (v, next) -> v : myUnfoldr f next
  Nothing        -> []

-- It helps to have the types in front of you
-- myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr (\a -> Just (a, f a))
