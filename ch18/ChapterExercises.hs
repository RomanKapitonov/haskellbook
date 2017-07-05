module ChapterExercises where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Nope a = NopeDotJpg deriving (Eq, Show)
data PhhhbbtttEither b a = Right' b | Left' a deriving (Eq, Show)
newtype Identity a = Identity a deriving (Eq, Ord, Show)
data List a = Nil | Cons a (List a) deriving (Eq, Show)

-- data Nope a = NopeDotJpg
----------------------------------------------------
instance Functor Nope where
  fmap _ NopeDotJpg = NopeDotJpg

instance Applicative Nope where
  pure = const NopeDotJpg
  NopeDotJpg <*> NopeDotJpg = NopeDotJpg

instance Monad Nope where
  return = pure
  NopeDotJpg >>= _ = NopeDotJpg

instance Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance EqProp (Nope a) where
  (=-=) = eq

-- data PhhhbbtttEither b a = Right' b | Left' a
----------------------------------------------------
instance Functor (PhhhbbtttEither b) where
  fmap _ (Right' b) = (Right' b)
  fmap f (Left' a) = Left' (f a)

instance Applicative (PhhhbbtttEither b) where
  pure = Left'
  (Right' a) <*> _ = Right' a
  _ <*> (Right' a) = Right' a
  (Left' f) <*> (Left' x) = Left' (f x)

instance Monad (PhhhbbtttEither b) where
  return = pure
  (Right' a) >>= _ = (Right' a)
  (Left' x) >>= f = (f x)

instance (Arbitrary b, Arbitrary a) => Arbitrary (PhhhbbtttEither b a) where
  arbitrary = oneof $ [Right' <$> arbitrary, Left' <$> arbitrary]

instance (Eq b, Eq a) => EqProp (PhhhbbtttEither b a) where
  (=-=) = eq

-- newtype Identity a = Identity a deriving (Eq, Ord, Show)
----------------------------------------------------
instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity x) = Identity (f x)

instance Monad Identity where
  return = pure
  (Identity x) >>= f = f x

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance (Eq a) => EqProp (Identity a) where
  (=-=) = eq

-- data List a = Nil | Cons a (List a)
----------------------------------------------------
instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Monoid (List a) where
  mempty = Nil
  mappend Nil ys = ys
  mappend (Cons x xs) ys = (Cons x (xs `mappend` ys))

instance Applicative List where
  pure x = Cons x Nil
  Nil <*> _   = Nil
  _   <*> Nil = Nil
  (Cons f fs) <*> xs = (f <$> xs) `mappend` (fs <*> xs)

instance Monad List where
  return = pure
  Nil >>= _ = Nil
  xs >>= f = concat $ fmap f xs
    where
      concat Nil = Nil
      concat (Cons x xs) = x `mappend` (concat xs)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = oneof [ Cons <$> arbitrary <*> arbitrary
                    , return Nil ]

instance (Eq a) => EqProp (List a) where
  (=-=) = eq

main :: IO ()
main = do
  quickBatch $ functor (undefined :: Nope (String, String, Int))
  quickBatch $ applicative (undefined :: Nope (String, String, Int))
  quickBatch $ monad (undefined :: Nope (String, String, Int))

  quickBatch $ functor (undefined :: PhhhbbtttEither String (String, String, Int))
  quickBatch $ applicative (undefined :: PhhhbbtttEither String (String, String, Int))
  quickBatch $ monad (undefined :: PhhhbbtttEither String (String, String, Int))

  quickBatch $ functor (undefined :: Identity (String, String, Int))
  quickBatch $ applicative (undefined :: Identity (String, String, Int))
  quickBatch $ monad (undefined :: Identity (String, String, Int))

  quickBatch $ functor (undefined :: List (String, String, Int))
  quickBatch $ applicative (undefined :: List (String, String, Int))
  quickBatch $ monad (undefined :: List (String, String, Int))

-- (j . j) [[[1, 2]], [[]], [[3]]]
-- j [[1, 2], [], [3]]
j :: Monad m => m (m a) -> m a
j m = m >>= id

-- return :: b -> m b
-- :t (undefined :: Monad m => a -> m a) . (undefined :: (a -> b))
-- (undefined :: Monad m => a -> m a) . (undefined :: (a -> b)) :: Monad m => a1 -> m a
-- :t (>>=)
-- (>>=) :: Monad m => m a -> (a -> m b) -> m b
--                     m >>= (return . f)

l1 :: Monad m => (a -> b) -> m a -> m b
l1 f m = m >>= (return . f)

l1' :: Monad m => (a -> b) -> m a -> m b
l1' f mx = mx >>= (\x -> return (f x))

-- *ChapterExercises> :t fmap
-- fmap :: Functor f => (a -> b) -> f a -> f b
l1'' :: Monad m => (a -> b) -> m a -> m b
l1'' = (<$>)

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f mx my = mx >>= (\x -> my >>= (\y -> return (f x y) ) )

l2' :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2' f mx my = f <$> mx <*> my

l2'' :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2'' = ((<*>) .) . (<$>)

a :: Monad m => m a -> m (a -> b) -> m b
a mx mf = mf >>= (\f -> mx >>= (\x -> return (f x)))

a' :: Monad m => m a -> m (a -> b) -> m b
a' mx mf = mf >>= \f -> f <$> mx

a'' :: Monad m => m a -> m (a -> b) -> m b
a'' mx mf = mf <*> mx

a''' :: Monad m => m a -> m (a -> b) -> m b
a'''= flip (<*>)

-- meh :: Monad m => m a -> (a -> m b) -> m (m b)
meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = return []
meh (x:xs) f = (f x) >>= \fx -> (fx:) <$> (meh xs f)

flipType :: (Monad m) => [m a] -> m [a]
flipType = flip meh id