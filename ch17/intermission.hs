-- liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
--                                  g
-- g ~ (,)
-- g ~ a -> b -> c ~ a -> b -> (a, b)
-- (,) :: a -> b -> (a, b)
-- a ~ a
-- b ~ b
-- c ~ (a, b)
-- liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
-- f ~ []
-- liftA2 :: (a -> b -> c) -> [a] -> [b] -> [c]
-- liftA2 :: (a -> b -> (a, b)) -> [a] -> [b] -> [(a, b)]


