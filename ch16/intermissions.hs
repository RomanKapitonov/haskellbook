-- 1. a -> a
-- a :: *
-- 2. a -> b a -> T (b a)
-- a :: *
-- b :: * -> *
-- T :: * -> *
-- 3. c a b -> c b a
-- c :: * -> * -> *

-- (.)  :: (a -> b) -> (r -> a) -> (r -> b)
-- fmap :: Functor f => (c -> d) -> (f c -> f d)
-- fmap :: Functor g => (x -> y) -> (g x -> g y)

-- (a -> b) ~ ((c -> d) -> (f c -> f d))
-- a ~ (c -> d)
-- b ~ (f c -> f d)

-- (a -> b) -> (r -> a) -> (r -> b) ~
-- (
--   --  a    ->      b
--   (c -> d) -> (f c -> f d) ->
--   -- r     ->   a
--   (r -> (c -> d)) ->
--   -- r     ->    b
--   (r -> (f c -> f d))
-- )

-- (r -> a) ~ (x -> y) -> (g x -> g y)

-- r ~ (x -> y)
-- a ~ (c -> d)
-- c ~ g x
-- d ~ g y

-- (c -> d) ~ (g x -> g y)

-- (a -> b) -> (r -> a) -> (r -> b) ~
-- (
--   --  a    ->      b
--   (g x -> g y) -> (f g x -> f g y) ->
--   -- r     ->   a
--   ((x -> y) -> (g x -> g y)) ->
--   -- r     ->    b
--   ((x -> y) -> (f g x -> f g y))
-- )

a = fmap (+1) $ read "[1]" :: [Int]
b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])
c = fmap (*2) (\x -> x - 2)
d = fmap ((return '1' ++) . show) (\x -> [x, 1..3])
e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        changed = fmap (fmap read (fmap ("123"++) show)) ioi
    in fmap (*3) changed
