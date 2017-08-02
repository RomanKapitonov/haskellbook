{-# LANGUAGE ScopedTypeVariables, InstanceSigs #-}

module Identity where

import Control.Applicative
import Control.Monad ()

newtype Identity a = Identity { runIdentity :: a }
newtype Compose f g a = Compose { runCompose :: f (g a) } deriving (Show, Eq)
data Hole = Hole
hole = undefined

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

newtype One f a = One (f a) deriving (Show, Eq)

instance Functor f => Functor (One f) where
  fmap f (One fa) = One $ fmap f fa

newtype Three f g h a = Three (f (g (h a)))

instance (Functor f, Functor g, Functor h) => Functor (Three f g h) where
  fmap f (Three fgh) = Three $ (fmap . fmap . fmap) f fgh

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
  pure a = Identity a
  Identity f <*> Identity a = Identity $ f a

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure a = Compose $ (pure . pure) a
  (Compose f) <*> (Compose a) = Compose $ (convert f) <*> a
    where convert :: f (g (a -> b)) -> f (g a -> g b)
          convert f' = (pure (<*>)) <*> f'

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
  foldMap f (Compose fg) = (foldMap . foldMap) f fg

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
  traverse f (Compose gh) = Compose <$> (traverse . traverse) f gh
