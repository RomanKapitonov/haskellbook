module Either where

import Data.Char

-- lefts' [Left 1, Left 2, Left 5, Right 'e']
lefts' :: [Either a b] -> [a]
lefts' = foldr fetch []
  where
    fetch (Left e) acc = e:acc
    fetch _        acc = acc

-- rights' [Right 'a', Right 'b', Right 'c', Left 1]
rights' :: [Either a b] -> [b]
rights' = foldr fetch []
  where
    fetch (Right v) acc = v:acc
    fetch _        acc = acc

-- partitionEithers' [Right 'a', Right 'b', Right 'c', Left 1]
partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' xs = (lefts' xs, rights' xs)

-- eitherMaybe' (+1) (Right 5)
-- eitherMaybe' (+1) (Left 'c')
eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f (Right v) = Just (f v)
eitherMaybe' _ _         = Nothing

-- either' ord (+1) (Left 'a')
-- either' ord (+1) (Right 100)
either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left e)  = f e
either' _ g (Right v) = g v

-- eitherMaybe'' (+1) (Right 5)
-- eitherMaybe'' (+5) (Left 'a')
eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f = either' (const Nothing) (Just . f)
