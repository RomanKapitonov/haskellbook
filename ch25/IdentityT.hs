module IdentityT where

newtype Identity a = Identity { runIdentity :: a } deriving (Eq, Show)
newtype IdentityT f a = IdentityT { runIdentityT :: f a } deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Functor f => Functor (IdentityT f) where
  fmap f (IdentityT fa) = IdentityT (fmap f fa)

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity a) = Identity (f a)

instance (Applicative m) => Applicative (IdentityT m) where
  pure x = IdentityT (pure x)
  (IdentityT f) <*> (IdentityT a) = IdentityT (f <*> a)

instance Monad Identity where
  return = pure
  (Identity a) >>= f = f a

instance (Monad m) => Monad (IdentityT m) where
  return = pure
  (IdentityT ma) >>= f = IdentityT $ ma >>= runIdentityT . f

:t runIdentityT
runIdentityT :: (IdentityT m a -> m a)

undefined :: (IdentityT m b -> m b) -> (a -> IdentityT m b) -> m b
runIdentityT :: 

runIdentity 

:t runIdentityT . f
(a -> IdentityT m b)

:t (>>=)
(>>=) :: Monad m => 

              m a               ->         (a -> m b)                    -> m b

:t (undefined :: IdentityT m a) >>= (undefined :: (a -> IdentityT m b))

(>>=) :: IdentityT m a -> (a -> IdentityT m b) -> IdentityT m b