{-# LANGUAGE ScopedTypeVariables, InstanceSigs #-}

module MonadTrans where

import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Monad

newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }
newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }
newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }
newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

data Hole = Hole
hole = undefined

instance MonadTrans (EitherT e) where
  lift = EitherT . liftM Right

instance MonadTrans (StateT s) where
  lift ma = StateT $ \s ->
    liftM (flip (,) s) ma

instance MonadTrans (MaybeT) where
  lift = MaybeT . liftM Just

instance MonadTrans (ReaderT r) where
  lift = ReaderT . const

instance (Functor m) => Functor (MaybeT m) where
  fmap f (MaybeT ma) = MaybeT $ (fmap . fmap) f ma

instance (Applicative m) => Applicative (MaybeT m) where
  pure x = MaybeT $ (pure (pure x))
  (MaybeT fn) <*> (MaybeT x) = MaybeT $ (convert fn) <*> x
    where convert :: m (Maybe (a -> b)) -> m (Maybe a -> Maybe b)
          convert h = (pure (<*>)) <*> h

instance (Monad m) => Monad (MaybeT m) where
  return = pure
  (MaybeT ma) >>= f =
    MaybeT $ do
      v <- ma
      case v of
        Nothing -> return Nothing
        Just y -> runMaybeT (f y)

instance Functor m => Functor (ReaderT r m) where
  fmap f (ReaderT rma) = ReaderT $ (fmap . fmap) f rma

instance Applicative m => Applicative (ReaderT r m) where
  pure x = ReaderT $ (pure . pure) x
  (ReaderT mf) <*> (ReaderT ma) = ReaderT $ (pure (<*>)) <*> mf <*> ma

instance Monad m => Monad (ReaderT r m) where
  return = pure
  (ReaderT rma) >>= f = ReaderT $ \r -> do
    a <- rma r
    runReaderT (f a) r

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

instance (MonadIO m) => MonadIO (MaybeT m) where
  liftIO = lift . liftIO

instance (MonadIO m) => MonadIO (ReaderT r m) where
  liftIO = lift . liftIO

instance (MonadIO m) => MonadIO (StateT s m) where
  liftIO = lift . liftIO
