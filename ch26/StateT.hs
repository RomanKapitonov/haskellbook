{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module StateT where

newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }
data Hole = Hole
hole = undefined

instance (Functor m) => Functor (StateT s m) where
  fmap :: (a -> b)
       -> StateT s m a
       -> StateT s m b
  fmap f (StateT sma) = StateT $ (fmap . fmap) (over f) sma
    where over :: (a -> b) -> (a, s) -> (b, s)
          over h (a, s) = (h a, s)

instance Monad m => (Applicative (StateT s m)) where
  pure a = StateT $ \s -> pure (a, s)
  (StateT smf) <*> (StateT sma) = StateT $ \s -> do
    (a, s1) <- sma s
    (f, s2) <- smf s1
    return (f a, s2)


instance Monad m => Monad (StateT s m) where
  return = pure
  (StateT sma) >>= f = StateT $ \s -> do
    (a, s1) <- sma s
    (runStateT $ f a) s1
