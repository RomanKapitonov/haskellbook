{-# LANGUAGE ScopedTypeVariables, InstanceSigs #-}

module MaybeT where

import Data.Maybe

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

data Hole = Hole
hole = undefined

instance (Functor m) => Functor (MaybeT m) where
  fmap f (MaybeT ma) = MaybeT $ (fmap . fmap) f ma

-- instance (Applicative m) => Applicative (MaybeT m) where
--   pure x = MaybeT $ pure (pure x)
--   (MaybeT fn) <*> (MaybeT x) = MaybeT $ (convert fn) <*> x
--     where convert :: m (Maybe (a -> b)) -> m (Maybe a -> Maybe b)
--             -- First option using (<*>) <$>
--           convert h = (<*>) <$> h

instance (Applicative m) => Applicative (MaybeT m) where
  pure x = MaybeT $ (pure (pure x))
  (MaybeT fn) <*> (MaybeT x) = MaybeT $ (convert fn) <*> x
    where convert :: m (Maybe (a -> b)) -> m (Maybe a -> Maybe b)
          -- Another option using (pure (<*>) <*>)
          convert h = (pure (<*>)) <*> h

instance (Monad m) => Monad (MaybeT m) where
  return = pure
  (MaybeT ma) >>= f =
    MaybeT $ do
      v <- ma
      case v of
        Nothing -> return Nothing
        Just y -> runMaybeT (f y)
