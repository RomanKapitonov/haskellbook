{-# LANGUAGE ScopedTypeVariables, InstanceSigs #-}

module EitherT where

newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }
data Hole = Hole
hole = undefined

instance (Functor m) => Functor (EitherT e m) where
  fmap f (EitherT a) = EitherT $ (fmap . fmap) f a

instance Applicative m => Applicative (EitherT e m) where
  pure a = EitherT $ (pure . pure) a
  (EitherT mf) <*> (EitherT ma) = EitherT $ (convert mf) <*> ma
    where convert :: m (Either e (a -> b)) -> m (Either e a -> Either e b)
          convert h = (pure (<*>)) <*> h

instance Monad m => Monad (EitherT e m) where
  return = pure
  (EitherT ma) >>= f = EitherT $ do
    a <- ma
    case a of
      Left e -> return (Left e)
      Right v -> runEitherT (f v)

swapEither :: Either e a -> Either a e
swapEither (Left e) = Right e
swapEither (Right a) = Left a

swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT (EitherT ema) = EitherT $ (swapEither <$> ema)

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' fa _ (Left e) = fa e
either' _ fb (Right v) = fb v

eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT fa fb (EitherT x) = x >>= either' fa fb
