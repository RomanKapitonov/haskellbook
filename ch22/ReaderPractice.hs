module ReaderPractice where

import Control.Applicative
import Data.Maybe
import Data.Monoid

x :: [Integer]
x = [1, 2, 3]

y :: [Integer]
y = [4, 5, 6]

z :: [Integer]
z = [7, 8, 9]

-- *ReaderPractice> First (Nothing) <> First (Just 1) <> First (Nothing)
-- First {getFirst = Just 1}

lookup' :: Eq a => a -> [(a, b)] -> Maybe b
lookup' = (getFirst .) . foldMap . (First .) . f
  where
    f x (a, b) = if x == a then Just b else Nothing

--zip x and y using 3 as the lookup key
xs :: Maybe Integer
xs = lookup' 3 $ zip x y

-- zip y and z using 6 as the lookup key
ys :: Maybe Integer
ys = lookup' 6 $ zip y z

-- it's also nice to have one that
-- will return Nothing, like this one
-- zip x and y using 4 as the lookup key
zs :: Maybe Integer
zs = lookup' 4 $ zip x y

-- now zip x and z using a variable lookup key
z' :: Integer -> Maybe Integer
z' n = lookup' n $ zip x z

x1 :: Maybe (Integer, Integer)
x1 = liftA2 (,) xs ys

x2 :: Maybe (Integer, Integer)
x2 = liftA2 (,) ys zs

x3 :: Integer -> (Maybe Integer, Maybe Integer)
x3 = liftA2 (,) z' z'

-- uncurry :: (a -> b -> c) -> (a, b) -> c
-- that first argument is a function
-- in this case, we want it to be addition
-- summed is just uncurry with addition as
-- the first argument

summed :: Num c => (c, c) -> c
summed = uncurry (+)

-- use &&, >3, <8
bolt :: Integer -> Bool
bolt = liftA2 (&&) (>3) (<8)

fromMaybe' :: a -> Maybe a -> a
fromMaybe' d Nothing  = d
fromMaybe' _ (Just x) = x


main :: IO ()
main = do
  print $ sequenceA [Just 3, Just 2, Just 1]
  print $ sequenceA [x, y]
  print $ sequenceA [xs, ys]
  print $ summed <$> ((,) <$> xs <*> ys)
  print $ fmap summed ((,) <$> xs <*> zs)
  print $ bolt 7
  print $ fmap bolt z
  print $ getAll . foldMap All $ sequA 7
  print $ getAny . foldMap Any $ sequA 7
  print $ sequA $ fromMaybe 0 s'
  print $ bolt $ fromMaybe 0 ys
  print $ bolt $ fromMaybe 0 $ z' 3

sequA :: Integral a => a -> [Bool]
sequA = sequenceA [(>3), (<8), even]

s' :: Maybe Integer
s' = summed <$> ((,) <$> xs <*> ys)
