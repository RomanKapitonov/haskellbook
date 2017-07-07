-- Funtor ((->) c)

-- fmap :: (a -> b) -> f a -> f b

-- f a ~ (c -> a)
-- f b ~ (c -> b)

-- fmap :: (a -> b) -> (c -> a) -> (c -> b)
-- (.)  :: (b -> c) -> (a -> b) -> (a -> c)

-- instance Funtor ((->) c) where
--   fmap = (.)

-- pure  :: Applicative f => a -> f a
-- (<*>) :: Applicative f => f (a -> b) -> f a -> f b

-- f ~ (r ->)
-- f a ~ (r -> a)

-- f (a -> b) ~ (r -> (a -> b))
-- f ~ (r ->)
-- f a ~ (r -> a)
-- <*> ~ (r -> (a -> b)) -> (r -> a) -> (r -> b)
-- <*> ~ (r -> a -> b) -> (r -> a) -> r -> b

-- instance Applicative ((->) r) where
--   pure x = \_ -> x
--   f <*> g = \e (f e (g e))

