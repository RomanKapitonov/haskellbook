module MyEither where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second (f b)

instance Applicative (Sum a) where
  pure = Second
  (First a) <*> _ = First a
  _ <*> (First a) = First a
  (Second f) <*> (Second x) = Second (f x)

instance Monad (Sum a) where
  return = pure
  (Second x) >>= f = f x
  (First e) >>= _ = First e

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = do
    oneof  [ First <$> arbitrary
           , Second <$> arbitrary ]

instance (Eq a, Eq b) => EqProp (Sum a b) where
  (=-=) = eq

main :: IO ()
main = do
  quickBatch $ monad (undefined :: Sum String (String, String, Int))
