module Bind where

import Control.Monad

-- fmap  :: Functor m => (a -> b) -> m a -> m b
-- b ~ m b
-- (a -> b) -> m a -> m b
-- (a -> m b) -> m a -> m (m b)
-- m (m a) -> m a
-- join [(a -> m b) -> m a -> m (m b)]
-- ((a -> m b) -> m a -> m (m b)) -> m a
-- join  :: Monad m => m (m a) -> m a
-- (>>=) :: Monad m => m a -> (a -> m b) -> m b

bind :: Monad m => (a -> m b) -> m a -> m b
bind f = join . fmap f
