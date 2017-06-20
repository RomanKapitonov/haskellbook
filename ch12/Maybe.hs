module Maybe where

-- >>> isJust (Just 1)
-- True
-- >>> isJust Nothing
-- False
isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing  = False

-- >>> isNothing (Just 1)
-- False
-- >>> isNothing Nothing
-- True
isNothing :: Maybe a -> Bool
isNothing Nothing  = True
isNothing (Just _) = False


-- >>> mayybee 0 (+1) Nothing
--0
-- >>> mayybee 0 (+1) (Just 1)
--2
mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee acc _ Nothing  = acc
mayybee acc f (Just x) = f x

-- >>> fromMaybe 0 Nothing --0
-- >>> fromMaybe 0 (Just 1) --1
fromMaybe :: a -> Maybe a -> a
fromMaybe acc x = mayybee acc id x
-- Try writing it in terms of the maybe catamorphism

-- >>> listToMaybe [1, 2, 3] -- Just 1
-- >>> listToMaybe []
-- Nothing
listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:_) = Just x

  -- >>> maybeToList (Just 1)
  -- [1]
  -- >>> maybeToList Nothing
  -- []
maybeToList :: Maybe a -> [a]
maybeToList Nothing  = []
maybeToList (Just x) = [x]

-- >>> catMaybes [Just 1, Nothing, Just 2]
-- [1, 2]
-- >>> catMaybes [Nothing, Nothing, Nothing]
-- []

catMaybes :: [Maybe a] -> [a]
catMaybes = foldr f []
  where
    f (Just x) acc = x:acc
    f Nothing  acc = acc

catMaybes' :: [Maybe a] -> [a]
catMaybes' [] = []
catMaybes' (Nothing:xs) = catMaybes' xs
catMaybes' ((Just x):xs) = x : catMaybes' xs

-- >>> flipMaybe [Just 1, Just 2, Just 3]
  -- Just [1, 2, 3]
  -- >>> flipMaybe [Just 1, Nothing, Just 3]
  -- Nothing
flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe = foldr f (Just [])
  where
    f (Just x) (Just acc) = Just (x : acc)
    f _ _                 = Nothing
