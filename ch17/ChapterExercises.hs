module ChapterExercises where

import Control.Applicative
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- -- Type
-- []
-- -- Methods
-- pure ::a -> [a]
-- (<*>) :: [(a -> b)] -> [a] -> [b]

-- -- Type
-- IO
-- -- Methods
-- pure :: a -> IO a
-- (<*>) :: IO (a -> b) -> IO a -> IO b

-- -- Type
-- (,) a
-- -- Methods
-- pure :: a -> (e, a)
-- (<*>) :: (e, (a -> b)) -> (e, a) -> (e, b)

-- -- Type
-- (->) e
-- -- Methods
-- pure :: a -> (e -> a)
-- (<*>) :: (e -> (a -> b)) -> (e -> a) -> (e -> b)

newtype Identity a = Identity a deriving (Show, Eq)
data Pair a = Pair a a deriving (Show, Eq)
data Two a b = Two a b deriving (Show, Eq)
data Three a b c = Three a b c deriving (Show, Eq)
data Three' a b = Three' a b b deriving (Show, Eq)
data Four a b c d = Four a b c d deriving (Show, Eq)
data Four' a b = Four' a a a b deriving (Show, Eq)

-- newtype Identity a = Identity a deriving (Show, Eq)
-------------------------------------------------
instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity x) = Identity (f x)

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return (Identity a)

instance (Eq a) => EqProp (Identity a) where
  (=-=) = eq

-- data Pair a = Pair a a deriving (Show, Eq)
-------------------------------------------------
instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance Applicative Pair where
  pure x = Pair x x
  (Pair f g) <*> (Pair x y) = Pair (f x) (g y)

instance (Arbitrary a) => Arbitrary (Pair a) where
  arbitrary = liftA2 Pair arbitrary arbitrary

instance (Eq a) => EqProp (Pair a) where
  (=-=) = eq

-- data Two a b = Two a b deriving (Show, Eq)
-------------------------------------------------
instance Functor (Two a) where
  fmap f (Two e x) = Two e (f x)

instance Monoid a => Applicative (Two a) where
  pure = Two mempty
  (Two e1 f) <*> (Two e2 x) = Two (e1 `mappend` e2) (f x)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = liftA2 Two arbitrary arbitrary

instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq

-- data Three a b c = Three a b c deriving (Show, Eq)
-------------------------------------------------

instance Functor (Three a b) where
  fmap f (Three a b x) = Three a b (f x)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure = Three mempty mempty
  (Three a1 b1 f) <*> (Three a2 b2 x) = Three (a1 `mappend` a2) (b1 `mappend` b2) (f x)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = liftA3 Three arbitrary arbitrary arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

-- data Three' a b = Three' a b b deriving (Show, Eq)
-------------------------------------------------
instance Functor (Three' a) where
  fmap f (Three' a x1 x2) = Three' a (f x1) (f x2)

instance (Monoid a) => Applicative (Three' a) where
  pure x = Three' mempty x x
  (Three' e1 f g) <*> (Three' e2 x1 x2) = Three' (e1 `mappend` e2) (f x1) (g x2)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = liftA3 Three' arbitrary arbitrary arbitrary

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq

-- data Four a b c d = Four a b c d deriving (Show, Eq)
-------------------------------------------------
instance Functor (Four a b c) where
  fmap f (Four e1 e2 e3 x) = Four e1 e2 e3 (f x)

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  pure = Four mempty mempty mempty
  (Four e11 e12 e13 f) <*> (Four e21 e22 e23 x) = Four (e11 `mappend` e21) (e12 `mappend` e22) (e13 `mappend` e23) (f x)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return (Four a b c d)

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
  (=-=) = eq

-- data Four' a b = Four' a a a b deriving (Show, Eq)
-------------------------------------------------
instance Functor (Four' a) where
  fmap f (Four' e1 e2 e3 x) = Four' e1 e2 e3 (f x)

instance Monoid a => Applicative (Four' a) where
  pure = Four' mempty mempty mempty
  (Four' e11 e12 e13 f) <*> (Four' e21 e22 e23 x) =
    Four' (e11 `mappend` e21) (e12 `mappend` e22) (e13 `mappend` e23) (f x)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return (Four' a b c d)

instance (Eq a, Eq b) => EqProp (Four' a b) where
  (=-=) = eq


main :: IO ()
main = do
  quickBatch $ functor (undefined :: Identity (String, String, Int))
  quickBatch $ applicative (undefined :: Identity (String, String, Int))

  quickBatch $ functor (undefined :: Pair (String, String, Int))
  quickBatch $ applicative (undefined :: Pair (String, String, Int))

  quickBatch $ functor (undefined :: Two String (String, String, Int))
  quickBatch $ applicative (undefined :: Two String (String, String, Int))

  quickBatch $ functor (undefined :: Three String String (String, String, Int))
  quickBatch $ applicative (undefined :: Three String String (String, String, Int))

  quickBatch $ functor (undefined :: Three' String (String, String, Int))
  quickBatch $ applicative (undefined :: Three' String (String, String, Int))

  quickBatch $ functor (undefined :: Four String String String (String, String, Int))
  quickBatch $ applicative (undefined :: Four String String String (String, String, Int))

  quickBatch $ functor (undefined :: Four' String (String, String, Int))
  quickBatch $ applicative (undefined :: Four' String (String, String, Int))

