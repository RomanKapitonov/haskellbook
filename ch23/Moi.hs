{-# LANGUAGE InstanceSigs #-}

module Moi where

newtype Moi s a = Moi { runMoi :: s -> (a, s) }

instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap f (Moi g) = Moi $ \s -> let (a, s') = g s
                               in (f a, s')

-- Alternative:
-- instance Functor (Moi s) where
--   fmap :: (a -> b) -> Moi s a -> Moi s b
--   fmap f (Moi g) = Moi $ \s -> (\(a, s') -> (f a, s')) (g s)

instance Applicative (Moi s) where
  pure :: a -> Moi s a
  pure a = Moi $ \s -> (a, s)
  (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
  (Moi f) <*> (Moi g) = Moi $ \s -> let (f', s') = f s
                                        (a, s'') = g s'
                                    in  (f' a, s'')

-- Alternative:
-- instance Applicative (Moi s) where
--   pure :: a -> Moi s a
--   pure a = Moi $ \s -> (a, s)
--   (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
--   (Moi f) <*> (Moi g) = Moi $ (\s ->
--       (\(f', s') ->
--         (\(a, s'') ->
--           (f' a, s'')
--         ) (g s')
--       ) (f s)
--     )

instance Monad (Moi s) where
  return = pure
  (Moi f) >>= g = Moi $ \s -> let (a, s') = f s
                                  ns = runMoi $ g a
                              in  ns s'

-- Alternative
-- instance Monad (Moi s) where
--   return = pure
--   (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
--   (Moi f) >>= g = Moi $ (\s ->
--       (\(a, s') ->
--         (\(Moi h) ->
--           h s'
--         ) (g a)
--       ) (f s)
--     )

-- let m1 = (Moi (\s -> ("", s)))
-- let f = \a -> (Moi (\s -> (a ++ show s, s)))
-- runMoi (m1 >>= f >>= f >>= f) 5
