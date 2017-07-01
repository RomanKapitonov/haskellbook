{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}

module Lib where

import GHC.Arr
import Test.QuickCheck

-- data Bool = False | True
-- Is may not be a valid instance of Functor since
-- it is a Nullary data constructor. * -> * is required

data BoolAndSomethingElse a = False' a | True' a deriving (Eq, Show)
data BoolAndMaybeSomethingElse a = Falsish | Truish a deriving (Eq, Show)
newtype Mu f = InF { outF :: f (Mu f) }
data D = D (Array Word Word) Int Int -- :: *
data Sum a b = First a | Second b
data Company a b c = DeepBlue a c | Something b
data More b a = L a b a | R b a b deriving (Eq, Show)
data Quant a b = Finance | Desk a | Bloor b
data K a b = K a
newtype Flip f a b = Flip (f b a) deriving (Eq, Show)
data EvilGoateeConst a b = GoatyConst b
data LiftItOut f a = LiftItOut (f a)
data Parappa f g a = DaWrappa (f a) (g a)
data IgnoreOne f g a b = IgnoringSomething (f a) (g b)
data Notorious g o a t = Notorious (g o) (g a) (g t)
data List a = Nil | Cons a (List a)
data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
data TalkToMe a = Halt | Print String a | Read (String -> a)

mumap :: Functor f => (forall a. f a -> g a) -> Mu f -> Mu g
mumap f (InF m) = InF $ f $ fmap (mumap f) m

instance Functor BoolAndSomethingElse where
  fmap f (True' a) = True' (f a)
  fmap f (False' a) = False' (f a)

instance Arbitrary a => Arbitrary (BoolAndSomethingElse a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    oneof $ return <$> [False' a, True' b]

instance Functor BoolAndMaybeSomethingElse where
  fmap _ Falsish = Falsish
  fmap f (Truish a) = Truish (f a)

instance Arbitrary a => Arbitrary (BoolAndMaybeSomethingElse a) where
  arbitrary = do
    a <- arbitrary
    oneof $ return <$> [Falsish, Truish a]

instance Functor (Sum e) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second (f b)

instance Functor (Company e e') where
  fmap _ (Something b) = Something b
  fmap f (DeepBlue a c) = DeepBlue a (f c)

instance Functor (More x) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'

instance Functor (Quant a) where
  fmap _ Finance  = Finance
  fmap _ (Desk a) = Desk a
  fmap f (Bloor b) = Bloor (f b)

instance Functor (K a) where
  fmap _ (K a) = (K a)

-- should remind you of an
-- instance you've written before
instance Functor (Flip K a) where
  fmap f (Flip (K x)) = Flip (K (f x))

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst (f b)

instance Functor f => Functor (LiftItOut f) where
  fmap f (LiftItOut x) = LiftItOut (fmap f x)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f (DaWrappa x y) = DaWrappa (fmap f x) (fmap f y)

instance Functor g => Functor (IgnoreOne f g a) where
  fmap f (IgnoringSomething x y) = IgnoringSomething x (fmap f y)

instance Functor g => Functor (Notorious g o a) where
  fmap f (Notorious x y z) = Notorious x y (fmap f z)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Functor GoatLord where
  fmap f NoGoat = NoGoat
  fmap f (OneGoat x) = OneGoat (f x)
  fmap f (MoreGoats x y z) = MoreGoats (fmap f x) (fmap f y) (fmap f z)

instance Functor TalkToMe where
  fmap _ Halt = Halt
  fmap f (Print a x) = Print a (f x)
  fmap f (Read g) = Read $ fmap f g