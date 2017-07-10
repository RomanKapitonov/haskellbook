{-# LANGUAGE InstanceSigs #-}

module HullDurr where

import Data.Char
import Control.Applicative (liftA2)

hurr = (*2)
durr = (+10)

m :: Integer -> Integer
m = hurr . durr

m2 :: Integer -> Integer
m2 = (+) <$> hurr <*> durr

m3 :: Integer -> Integer
m3 = liftA2 (+) hurr durr

cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = cap . rev

fmapped :: [Char] -> [Char]
fmapped = fmap cap rev

tupled :: String -> (String, String)
tupled = liftA2 (,) cap rev

tupled' :: String -> (String, String)
tupled' = do
  l <- rev
  r <- cap
  return (l, r)

tupled'' :: [Char] -> ([Char], [Char])
tupled'' = (\x -> return (rev x, cap x)) =<< id

newtype Reader r a = Reader { runReader :: r -> a }

ask :: Reader a a
ask = Reader id

newtype HumanName = HumanName String deriving (Eq, Show)
newtype DogName = DogName String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)


data Person = Person {
    humanName :: HumanName
  , dogName :: DogName
  , address :: Address
} deriving (Eq, Show)

data Dog = Dog {
    dogsName :: DogName
  , dogsAddress :: Address
} deriving (Eq, Show)

pers :: Person
pers = Person (HumanName "Big Bird")
              (DogName "Barkley")
              (Address "Sesame Street")

chris :: Person
chris = Person (HumanName "Chris Allen")
               (DogName "Papu")
               (Address "Austin")

-- without Reader
getDog :: Person -> Dog
getDog p = Dog (dogName p) (address p)

-- with Reader
getDogR :: Person -> Dog
getDogR = Dog <$> dogName <*> address

getDogR' :: Person -> Dog
getDogR' = liftA2 Dog dogName address

myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f x y = f <$> x <*> y

asks :: (r -> a) -> Reader r a
asks f = Reader f

instance Functor (Reader r) where
  fmap f (Reader r) = Reader (f . r)

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader $ (\_ -> a)

  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  -- (Reader rab) <*> (Reader ra) = Reader (\r -> rab r (ra r))
  -- OR
  (Reader rab) <*> (Reader ra) = Reader (rab <*> ra)

getDogR'' :: Reader Person Dog
getDogR'' = Reader getDog

getDogR''' :: Reader Person Dog
getDogR''' = Dog <$> Reader dogName <*> Reader address

foo :: (Functor f, Num a) => f a -> f a
foo r = fmap (+1) r

bar :: Foldable f => t -> f a -> (t, Int)
bar r t = (r, length t)

froot :: Num a => [a] -> ([a], Int)
froot r = (map (+1) r, length r)

barOne :: Foldable t => t a -> (t a, Int)
barOne r = (r, length r)

frooty :: Num a => [a] -> ([a], Int)
frooty r = bar (foo r) r

frooty' :: Num a => [a] -> ([a], Int)
frooty' = \r -> bar (foo r) r

fooBind :: (r -> a) -> (a -> r -> b) -> (r -> b)
fooBind m k = \r -> k (m r ) r

-- with Reader Monad
getDogRM :: Person -> Dog
getDogRM = do
  name <- dogName
  addy <- address
  return $ Dog name addy

instance Monad (Reader r) where
  return = pure
  (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  -- ra       ~ (r -> a)
  -- ra r     ~ (r -> a) r ~ a
  -- f        ~ a -> (r -> a)
  -- f (ra r) ~ (a -> (r -> a)) a ~ (r -> a)
  (Reader ra) >>= f = Reader $ \r -> runReader (f (ra r)) r

-- with Reader Monad
getDogRM' :: Reader Person Dog
getDogRM' = do
  -- name <- dogName ~ (r -> a)
  name <- Reader dogName
  -- addy <- address ~ (r -> a)
  addy <- Reader address
  return $ Dog name addy
