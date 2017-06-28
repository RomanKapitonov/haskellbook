module SemigroupsSpec where

import Data.Semigroup

type Associativity x = x -> x -> x -> Bool

semigroupAssoc :: (Eq m, Semigroup m) => Associativity m
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

