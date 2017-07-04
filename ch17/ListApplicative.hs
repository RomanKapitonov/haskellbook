module ListApplicative where

import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data List a = Nil | Cons a (List a) deriving (Eq, Show)
newtype ZipList' a = ZipList' (List a) deriving (Eq, Show)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = oneof [ Cons <$> arbitrary <*> arbitrary
                    , return Nil ]

instance (Eq a) => EqProp (List a) where
  (=-=) = eq

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Monoid (List a) where
  mempty = Nil
  mappend Nil ys = ys
  mappend (Cons x xs) ys = (Cons x (xs `mappend` ys))

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = ZipList' <$> arbitrary

instance Applicative List where
  pure x = Cons x Nil
  Nil <*> _   = Nil
  _   <*> Nil = Nil
  (Cons f fs) <*> xs = (f <$> xs) <> (fs <*> xs)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs in take' 3000 l
          ys' = let (ZipList' l) = ys in take' 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure x = ZipList' (Cons x Nil)
  (ZipList' fs) <*> (ZipList' xs) = ZipList' (fs <*> xs)

take' :: Int -> List a -> List a
take' n c
  | n <= 0 = Nil
  | otherwise = case c of
      (Cons x xs) -> Cons x (take' (n - 1) xs)
      Nil         -> Nil

repeat' :: a -> List a
repeat' x = Cons x (repeat' x)

main :: IO ()
main = do
  quickBatch $ applicative (undefined :: List (String, String, Int))
  quickBatch $ monoid (undefined :: List (String, String, Int))
  quickBatch $ functor (undefined :: List (String, String, Int))

  quickBatch $ applicative (undefined :: ZipList' (String, String, Int))

