module ChapterExercises where

import Data.Monoid
import Data.Foldable

data Constant a b = Constant a
data Two a b = Two a b
data Three a b c = Three a b c
data Three' a b = Three' a b b
data Four' a b = Four' a b b b

instance Foldable (Constant a) where
  foldMap f (Constant a) = mempty

instance Foldable (Two a) where
  foldMap f (Two a b) = f b

instance Foldable (Three a b) where
  foldMap f (Three a b c) = f c

instance Foldable (Three' a) where
  foldMap f (Three' a b1 b2) = f b1 <> f b2

instance Foldable (Four' a) where
  foldMap f (Four' a b1 b2 b3) = f b1 <> f b2 <> f b3

filterF :: (Applicative f, Foldable f, Monoid (f a)) => (a -> Bool) -> f a -> f a
filterF f = foldMap (\x -> if f x then pure x else mempty)
