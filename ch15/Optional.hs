module Optional where

import Data.Monoid
import Test.QuickCheck
import Control.Monad (liftM)

data Optional a = Nada | Only a deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend (Only x) (Only y) = Only (x `mappend` y)
  mappend (Only x) (Nada)   = Only x
  mappend (Nada)   (Only x) = Only x
  mappend _        _        = Nada

arbitrary1 :: (Arbitrary1 f, Arbitrary a) => Gen (f a)
arbitrary1 = liftArbitrary arbitrary

shrink1 :: (Arbitrary1 f, Arbitrary a) => f a -> [f a]
shrink1 = liftShrink shrink

class Arbitrary1 f where
  liftArbitrary :: Gen a -> Gen (f a)
  liftShrink    :: (a -> [a]) -> f a -> [f a]
  liftShrink _ _ = []

instance Arbitrary1 Optional where
  liftArbitrary arb = frequency [(1, return Nada), (1, liftM Only arb)]
  liftShrink shr (Only x) = Nada : [ Only x' | x' <- shr x ]
  liftShrink _   Nada  = []

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = arbitrary1
  shrink = shrink1
