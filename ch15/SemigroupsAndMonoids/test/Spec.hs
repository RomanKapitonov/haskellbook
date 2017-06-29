module Main where

import SemigroupsAndMonoids
import SemigroupsAndMonoidsSpec
import Test.QuickCheck

main :: IO ()
main = do
  quickCheck (semigroupAssoc :: Associativity Trivial)
  quickCheck (monoidLeftIdentity :: LeftIdentity Trivial)
  quickCheck (monoidRightIdentity :: RightIdentity Trivial)

  quickCheck (semigroupAssoc :: Associativity (Identity String))
  quickCheck (monoidLeftIdentity :: LeftIdentity (Identity String))
  quickCheck (monoidRightIdentity :: RightIdentity (Identity String))

  quickCheck (semigroupAssoc :: Associativity (Two String String))
  quickCheck (monoidLeftIdentity :: LeftIdentity (Two String String))
  quickCheck (monoidRightIdentity :: RightIdentity (Two String String))

  quickCheck (semigroupAssoc :: Associativity (Three String String String))
  quickCheck (monoidLeftIdentity :: LeftIdentity (Three String String String))
  quickCheck (monoidRightIdentity :: RightIdentity (Three String String String))

  quickCheck (semigroupAssoc :: Associativity (Four String String String String))
  quickCheck (monoidLeftIdentity :: LeftIdentity (Four String String String String))
  quickCheck (monoidRightIdentity :: RightIdentity (Four String String String String))

  quickCheck (semigroupAssoc :: Associativity BoolConj)
  quickCheck (monoidLeftIdentity :: LeftIdentity BoolConj)
  quickCheck (monoidRightIdentity :: RightIdentity BoolConj)

  quickCheck (semigroupAssoc :: Associativity BoolDisj)
  quickCheck (monoidLeftIdentity :: LeftIdentity BoolDisj)
  quickCheck (monoidRightIdentity :: RightIdentity BoolDisj)

  quickCheck (semigroupAssoc :: Associativity (Or String String))

  quickCheck (semigroupCombineAssoc :: FunctionAssociativity (Combine Int String) Int)
  quickCheck (monoidLeftIdentityCombine :: FunctionIdentity (Combine Int String) Int)
  quickCheck (monoidRightIdentityCombine :: FunctionIdentity (Combine Int String) Int)

  quickCheck (semigroupCompAssoc :: FunctionAssociativity (Comp String) String)
  quickCheck (semigroupAssoc :: Associativity (Validation String String))
  quickCheck (semigroupAssoc :: Associativity (AccumulateRight String String))
  quickCheck (semigroupAssoc :: Associativity (AccumulateBoth String String))

  quickCheck (monoidMemAssoc :: FunctionAssociativity (Mem String String) String)
  quickCheck (monoidLeftIdentityMem :: FunctionIdentity (Mem String String) String)
  quickCheck (monoidRightIdentityMem :: FunctionIdentity (Mem String String) String)