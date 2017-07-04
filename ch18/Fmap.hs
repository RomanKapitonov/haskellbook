module Fmap where

-- fmap ::       Functor f => (a -> b) -> f a -> f b
-- liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
-- liftM2 ::       Monad m => (a1 -> a2 -> r) -> m a1 -> m a2 -> m r

twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
  x <- xs
  if even x
  then [x*x, x*x]
  else [x*x]

