module SemigroupsSpec where

import Semigroups
import Data.Semigroup

type Associativity x = x -> x -> x -> Bool
type FunctionAssociativity x c = x -> x -> x -> c -> Bool

semigroupAssoc :: (Eq a, Semigroup a) => Associativity a
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

semigroupCombineAssoc :: (Eq b, Semigroup b) => FunctionAssociativity (Combine a b) a
semigroupCombineAssoc f g h x = unCombine (f <> (g <> h)) x == unCombine ((f <> g) <> h) x

semigroupCompAssoc :: (Eq a, Semigroup a) => FunctionAssociativity (Comp a) a
semigroupCompAssoc f g h x = unComp (f <> (g <> h)) x == unComp ((f <> g) <> h) x
