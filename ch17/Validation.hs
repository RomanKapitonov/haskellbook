module Validation where

import Test.QuickCheck hiding (Error, Success)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Sum a b = First a | Second b deriving (Eq, Show)
data Validation e a = Error e | Success a deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second (f b)

instance Applicative (Sum a) where
  pure = Second
  (Second f) <*> (Second x) = Second (f x)
  _ <*> (First x) = First x
  (First f) <*> _ = First f

instance (Arbitrary e, Arbitrary b) => Arbitrary (Sum e b) where
  arbitrary = do
    e <- arbitrary
    b <- arbitrary
    oneof $ return <$> [ First e
                       , Second b ]

instance (Eq a, Eq b) => EqProp (Sum a b) where
  (=-=) = eq

-- same as Sum/Either
instance Functor (Validation e) where
  fmap _ (Error e) = Error e
  fmap f (Success x) = Success (f x)

-- This is different
instance Monoid e => Applicative (Validation e) where
  pure = Success
  (Error x) <*> (Error y) = Error (x `mappend` y)
  (Error x) <*> _ = Error x
  _ <*> (Error y) = Error y
  (Success f) <*> (Success x) = Success (f x)

instance (Arbitrary e, Arbitrary b) => Arbitrary (Validation e b) where
  arbitrary = do
    e <- arbitrary
    b <- arbitrary
    oneof $ return <$> [ Error e
                       , Success b ]

instance (Eq a, Eq b) => EqProp (Validation a b) where
  (=-=) = eq

main :: IO ()
main = do
  quickBatch $ functor (undefined :: Sum Int (String, String, Int))
  quickBatch $ applicative (undefined :: Sum Int (String, String, Int))

  quickBatch $ functor (undefined :: Validation String (String, String, Int))
  quickBatch $ applicative (undefined :: Validation String (String, String, Int))
