module Generators where

import Test.QuickCheck

data Fool = Fulse | Frue deriving (Eq, Show)

equalProbFoolGen :: Gen Fool
equalProbFoolGen = elements [Fulse, Frue]


twoThirdProbFoolGen :: Gen Fool
twoThirdProbFoolGen = do
  frequency [ (1, return Frue)
            , (2, return Fulse) ]
