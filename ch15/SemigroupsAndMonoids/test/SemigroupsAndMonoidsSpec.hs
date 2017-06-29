module SemigroupsAndMonoidsSpec where

import SemigroupsAndMonoids
import Data.Semigroup

type Associativity x = x -> x -> x -> Bool
type FunctionAssociativity f v = f -> f -> f -> v -> Bool
type LeftIdentity x = x -> Bool
type RightIdentity x = x -> Bool
type FunctionIdentity f x = f -> x -> Bool

semigroupAssoc :: (Eq a, Semigroup a) => Associativity a
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

semigroupCombineAssoc :: (Eq b, Semigroup b) => FunctionAssociativity (Combine a b) a
semigroupCombineAssoc f g h x = unCombine (f <> (g <> h)) x == unCombine ((f <> g) <> h) x

semigroupCompAssoc :: (Eq a, Semigroup a) => FunctionAssociativity (Comp a) a
semigroupCompAssoc f g h x = unComp (f <> (g <> h)) x == unComp ((f <> g) <> h) x

monoidLeftIdentity :: (Monoid m, Semigroup m, Eq m) => LeftIdentity m
monoidLeftIdentity x = mempty <> x == x

monoidRightIdentity :: (Monoid m, Semigroup m, Eq m) => RightIdentity m
monoidRightIdentity x = x <> mempty == x

monoidLeftIdentityCombine :: (Eq b, Semigroup b, Monoid b) => FunctionIdentity (Combine a b) a
monoidLeftIdentityCombine f x = unCombine (mempty <> f) x == unCombine f x

monoidRightIdentityCombine :: (Eq b, Semigroup b, Monoid b) => FunctionIdentity (Combine a b) a
monoidRightIdentityCombine f x = unCombine (f <> mempty) x == unCombine f x

monoidMemAssoc :: (Eq a, Eq s, Monoid a) => FunctionAssociativity (Mem s a) s
monoidMemAssoc f g h x = runMem ((f `mappend` g) `mappend` h) x == runMem (f `mappend` (g `mappend` h)) x

monoidLeftIdentityMem :: (Eq a, Eq s, Monoid a) => FunctionIdentity (Mem s a) s
monoidLeftIdentityMem f s = runMem (mempty `mappend` f) s == runMem f s

monoidRightIdentityMem :: (Eq a, Eq s, Monoid a) => FunctionIdentity (Mem s a) s
monoidRightIdentityMem f s = runMem (f `mappend` mempty) s == runMem f s
