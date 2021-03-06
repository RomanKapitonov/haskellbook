module Constant where

import Data.Monoid

newtype Constant a b =
  Constant { getConstant :: a }
  deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant e) = Constant e

instance Monoid a => Applicative (Constant a) where
  pure = const $ Constant mempty
  (Constant x) <*> (Constant y) = Constant (x <> y)
