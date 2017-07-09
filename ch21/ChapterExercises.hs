module ChapterExercises where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

newtype Identity a = Identity a deriving (Eq, Ord, Show)
newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Show)
data Optional a = Nada | Yep a deriving (Eq, Show)
data List a = Nil | Cons a (List a) deriving (Eq, Show)
data Three a b c = Three a b c deriving (Eq, Show)
data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Foldable Identity where
  foldMap f (Identity a) = f a

instance Traversable Identity where
  traverse f (Identity a) = Identity <$> f a

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance (Eq a) => EqProp (Identity a) where
  (=-=) = eq

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Foldable (Constant a) where
  foldMap _ _ = mempty

instance Traversable (Constant a) where
  traverse _ (Constant a) = pure (Constant a)

instance (Arbitrary a) => Arbitrary (Constant a b) where
  arbitrary = Constant <$> arbitrary

instance (Eq a) => EqProp (Constant a b) where
  (=-=) = eq

instance Functor Optional where
  fmap _ Nada = Nada
  fmap f (Yep a) = Yep (f a)

instance Foldable Optional where
  foldMap _ Nada = mempty
  foldMap f (Yep a) = f a

instance Traversable (Optional) where
  traverse _ Nada = pure Nada
  traverse f (Yep a) = Yep <$> f a

instance (Arbitrary a) => Arbitrary (Optional a) where
  arbitrary = do
    oneof [return Nada, Yep <$> arbitrary]

instance (Eq a) => EqProp (Optional a) where
  (=-=) = eq

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a r) = Cons (f a) (fmap f r)

instance Foldable List where
  foldMap f Nil = mempty
  foldMap f (Cons a r) = f a `mappend` foldMap f r

instance Traversable List where
  traverse _ Nil = pure Nil
  traverse f (Cons a r) = Cons <$> f a <*> traverse f r

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = oneof [ Cons <$> arbitrary <*> arbitrary
                    , return Nil ]

instance (Eq a) => EqProp (List a) where
  (=-=) = eq

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance Foldable (Three a b) where
  foldMap f (Three a b c) = f c

instance Traversable (Three a b) where
  traverse f (Three a b c) = Three a b <$> f c

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

instance Functor (Three' a) where
  fmap f (Three' a b1 b2) = Three' a (f b1) (f b2)

instance Foldable (Three' a) where
  foldMap f (Three' a b1 b2) = f b1 `mappend` f b2

instance Traversable (Three' a) where
  traverse f (Three' a b1 b2) = Three' a <$> f b1 <*> f b2

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = Three' <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq

main :: IO ()
main = do
  quickBatch $ traversable (undefined :: Identity (String, String, String))
  quickBatch $ traversable (undefined :: Constant String (String, String, String))
  quickBatch $ traversable (undefined :: Optional (String, String, String))
  quickBatch $ traversable (undefined :: List (String, String, String))
  quickBatch $ traversable (undefined :: Three String String (String, String, String))
  quickBatch $ traversable (undefined :: Three' String (String, String, String))
