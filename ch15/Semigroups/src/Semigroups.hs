{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Semigroups where

import Test.QuickCheck hiding (Success, Failure)
import Data.Semigroup
import Text.Show.Functions

data Trivial = Trivial deriving (Eq, Show)
data Two a b = Two a b deriving (Eq, Show)
data Three a b c = Three a b c deriving (Eq, Show)
data Four a b c d = Four a b c d deriving (Eq, Show)
newtype BoolConj = BoolConj Bool deriving (Eq, Show)
newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)
data Or a b = Fst a | Snd b deriving (Eq, Show)
newtype Combine a b = Combine { unCombine :: (a -> b) }
newtype Comp a = Comp { unComp :: (a -> a) }
data Validation a b = Failure a | Success b deriving (Eq, Show)
newtype AccumulateRight a b = AccumulateRight (Validation a b) deriving (Eq, Show)
newtype AccumulateBoth a b = AccumulateBoth (Validation a b) deriving (Eq, Show)


-- Prelude>let f = Combine $ \n -> Sum (n + 1)
-- Prelude>let g = Combine $ \n -> Sum (n - 1)
-- Prelude>unCombine (f <> g) $ 0
-- Sum {getSum = 0}
--     Prelude> unCombine (f <> g) $ 1
--     Sum {getSum = 2}
--     Prelude> unCombine (f <> f) $ 1
--     Sum {getSum = 4}
--     Prelude> unCombine (g <> f) $ 1
--     Sum {getSum = 2}

instance Arbitrary Trivial where
  arbitrary = return Trivial

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool
type IdentityAssoc a = Identity a -> Identity a -> Identity a -> Bool

instance Semigroup Trivial where
  Trivial <> Trivial = Trivial

newtype Identity a = Identity a deriving (Eq, Show)

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = do
   x <- arbitrary
   return (Identity x)

instance (Semigroup a) => Semigroup (Identity a) where
  (Identity x) <> (Identity y) = Identity (x <> y)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two a b) <> (Two c d) = Two (a <> c) (b <> d)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
  (Three a b c) <> (Three d e f) = Three (a <> d) (b <> e) (c <> f)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three a b c)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (Four a b c d) where
  (Four a b c d) <> (Four w x y z) = Four (a <> w) (b <> x) (c <> y) (d <> z)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return (Four a b c d)

instance Semigroup BoolConj where
  (BoolConj a) <> (BoolConj b) = BoolConj (a && b)

instance Arbitrary BoolConj where
  arbitrary = do
    a <- arbitrary
    return (BoolConj a)

instance Semigroup BoolDisj where
  (BoolDisj a) <> (BoolDisj b) = BoolDisj (a || b)

instance Arbitrary BoolDisj where
  arbitrary = do
    a <- arbitrary
    return (BoolDisj a)

-- Prelude> Fst 1 <> Snd 2
-- Snd 2
-- Prelude> Fst 1 <> Fst 2
-- Fst 2
-- Prelude> Snd 1 <> Fst 2
-- Snd 1
-- Prelude> Snd 1 <> Snd 2
-- Snd 1
instance (Semigroup a, Semigroup b) => Semigroup (Or a b) where
  (Fst _) <> x = x
  (Snd x) <> _ = Snd x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    oneof $ return <$> [Fst a, Snd b]

instance (Semigroup b) => Semigroup (Combine a b) where
  (Combine f) <> (Combine g) = Combine $ \x -> (f x) <> (g x)

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = do
    f <- arbitrary
    return $ Combine f

instance Show (Combine a b) where
  show (Combine f) = "Combine " ++ show f

instance (Semigroup a) => Semigroup (Comp a) where
  (Comp f) <> (Comp g) = Comp (f . g)

instance (CoArbitrary a, Arbitrary a) => Arbitrary (Comp a) where
  arbitrary = do
    f <- arbitrary
    return $ Comp f

instance Show (Comp a) where
  show (Comp f) = "Comp " ++ show f

instance Semigroup a => Semigroup (Validation a b) where
  Failure a <> Failure b = Failure $ a <> b
  Failure a <> Success _ = Failure a
  Success _ <> a = a

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    oneof $ return <$> [Failure a, Success b]

instance (Semigroup a, Semigroup b) => Semigroup (AccumulateRight a b) where
  AccumulateRight l <> AccumulateRight r = AccumulateRight (f (l, r))
    where
      f (Success a, Success b) = Success $ a <> b
      f (Success _, Failure b) = Failure b
      f (Failure a, _)         = Failure a

instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateRight a b) where
  arbitrary = AccumulateRight <$> arbitrary

instance (Semigroup a, Semigroup b) => Semigroup (AccumulateBoth a b) where
  AccumulateBoth l <> AccumulateBoth r = AccumulateBoth (f (l, r))
    where
      f (Success a, Success b) = Success $ a <> b
      f (Failure a, Failure b) = Failure $ a <> b
      f (Failure a, _)         = Failure a
      f (_, Failure b)         = Failure b

instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateBoth a b) where
  arbitrary = AccumulateBoth <$> arbitrary
