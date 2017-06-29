module Main where

import Semigroups
import SemigroupsSpec
import Test.QuickCheck

main :: IO ()
main = do
  quickCheck (semigroupAssoc :: Associativity Trivial)
  quickCheck (semigroupAssoc :: Associativity (Identity String))
  quickCheck (semigroupAssoc :: Associativity (Two String String))
  quickCheck (semigroupAssoc :: Associativity (Three String String String))
  quickCheck (semigroupAssoc :: Associativity (Four String String String String))
  quickCheck (semigroupAssoc :: Associativity BoolConj)
  quickCheck (semigroupAssoc :: Associativity BoolDisj)
  quickCheck (semigroupAssoc :: Associativity (Or String String))
  quickCheck (semigroupCombineAssoc :: FunctionAssociativity (Combine Int String) Int)
  quickCheck (semigroupCompAssoc :: FunctionAssociativity (Comp String) String)
  quickCheck (semigroupAssoc :: Associativity (Validation String String))
  quickCheck (semigroupAssoc :: Associativity (AccumulateRight String String))
  quickCheck (semigroupAssoc :: Associativity (AccumulateBoth String String))
