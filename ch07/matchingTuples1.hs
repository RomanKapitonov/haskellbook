module TupleFunctions where

-- These have to be the same type because -- (+) is a -> a -> a
addEmUp2 :: Num a => (a, a) -> a
addEmUp2 (x, y) = x + y

-- addEmUp2 could also be written like so
addEmUp2Alt :: Num a => (a, a) -> a
addEmUp2Alt tup = (fst tup) + (snd tup)

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

third3 :: (a, b, c) -> c
third3 (_, _, x) = x

k (x, y) = x
k1 = k ((4-1), 10)
k2 = k ("three", (1 + 2))
k3 = k (3, True)

f :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
f (a, b, c) (x, y, z)= ((a, x), (c, z))

funcZ x =
  case x + 1 == 1 of
    True -> "AWESOME"
    False -> "wut"


pal xs = case xs == reverse xs of
  True -> "yes"
  False -> "no"

functionC x y = if (x > y) then x else y
functionC' x y = case x > y of
  True -> x
  False -> y

ifEvenAdd2 n = if even n then (n + 2) else n
ifEvenAdd2' n = case check of
  True -> (n+2)
  False -> n
  where check = even n

nums x = case compare x 0 of
  LT -> -1
  EQ -> 0
  GT -> 1

dodgy x y = x + y * 10
oneIsOne = dodgy 1
oneIsTwo = (flip dodgy) 2
