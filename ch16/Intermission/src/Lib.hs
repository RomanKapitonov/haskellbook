module Lib where

import Test.QuickCheck

newtype Identity a = Identity a deriving (Show, Eq)
data Pair a = Pair a a deriving (Show, Eq)
data Two a b = Two a b deriving (Show, Eq)
data Three a b c = Three a b c deriving (Show, Eq)
data Three' a b = Three' a b b deriving (Show, Eq)
data Four a b c d = Four a b c d deriving (Show, Eq)
data Four' a b = Four' a a a b deriving (Show, Eq)
-- Not possible due to no type argument present in data definition
-- Hence no structure to preserve when being referred to as Functor
data Trivial = Trivial
data Possibly a = LolNope | Yeppers a deriving (Eq, Show)
data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return (Identity a)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance (Arbitrary a) => Arbitrary (Pair a) where
  arbitrary = do
    a <- arbitrary
    return (Pair a a)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three a b c)

instance Functor (Three' a) where
  fmap f (Three' a b c) = Three' a (f b) (f c)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three' a b c)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return (Four a b c d)

instance Functor (Four' a) where
  fmap f (Four' a b c d) = Four' a b c (f d)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return (Four' a b c d)

instance Functor Possibly where
  fmap _ (LolNope) = LolNope
  fmap f (Yeppers x) = Yeppers (f x)

instance (Arbitrary a) => Arbitrary (Possibly a) where
  arbitrary = do
    a <- arbitrary
    oneof $ return <$> [LolNope, Yeppers a]

instance Functor (Sum a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    oneof $ return <$> [First a, Second b]
