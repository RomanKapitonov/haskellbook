module Main where

import Lib
import Data.List (sort)
import Test.QuickCheck
import Text.Show.Functions

prop_halfIdentity :: Property
prop_halfIdentity = forAll
  (arbitrary :: Gen Double)
  (\x -> halfIdentity x == x)

prop_listOrdered :: Property
prop_listOrdered = forAll
  (arbitrary :: Gen [Int])
  (listOrdered . sort)

prop_plusAssociative :: Property
prop_plusAssociative = forAll
  (arbitrary :: Gen Int)
  (plusAssociative)

prop_plusCommutative :: Property
prop_plusCommutative = forAll
  (arbitrary :: Gen Int)
  (plusCommutative)

prop_multiplyAssociative :: Property
prop_multiplyAssociative = forAll
  (arbitrary :: Gen Int)
  (multiplyAssociative)

prop_multiplyCommutative :: Property
prop_multiplyCommutative = forAll
  (arbitrary :: Gen Int)
  (multiplyCommutative)

prop_quotRule :: Property
prop_quotRule =
  forAll (getPositive <$> (arbitrary :: Gen (Positive Int))) $ \x ->
  forAll (getPositive <$> (arbitrary :: Gen (Positive Int))) $ \y ->
  (quotRule x y)

prop_divModRule :: Property
prop_divModRule =
  forAll (getPositive <$> (arbitrary :: Gen (Positive Int))) $ \x ->
  forAll (getPositive <$> (arbitrary :: Gen (Positive Int))) $ \y ->
  (divModRule x y)

prop_powerCommutative :: Property
prop_powerCommutative =
  forAll (getPositive <$> (arbitrary :: Gen (Positive Int))) $ \x ->
  forAll (getPositive <$> (arbitrary :: Gen (Positive Int))) $ \y ->
  (powerCommutative x y)

prop_powerAssociative :: Property
prop_powerAssociative =
  forAll (getPositive <$> (arbitrary :: Gen (Positive Int))) $ \x ->
  forAll (getPositive <$> (arbitrary :: Gen (Positive Int))) $ \y ->
  forAll (getPositive <$> (arbitrary :: Gen (Positive Int))) $ \z ->
  (powerAssociative x y z)

prop_reverseRule :: Property
prop_reverseRule = forAll
  (arbitrary :: Gen [Int])
  (reverseRule)

prop_functionApplication :: Property
prop_functionApplication =
  forAll (arbitrary :: Gen (Int -> Int)) $ \f ->
  forAll (arbitrary :: Gen Int) $ \x ->
  (functionApplication f x)

prop_functionComposition :: Property
prop_functionComposition =
  forAll (arbitrary :: Gen (Char -> Int)) $ \f ->
  forAll (arbitrary :: Gen (Bool -> Char)) $ \g ->
  forAll (arbitrary :: Gen Bool) $ \x ->
  (functionComposition f g x)

-- consConcatRule :: (Eq a) => [a] -> [a] -> Bool
-- consConcatRule xs ys = foldr (:) xs ys == xs ++ ys

prop_consConcatRule :: Property
prop_consConcatRule =
  forAll (arbitrary :: Gen [Int]) $ \xs ->
  forAll (arbitrary :: Gen [Int]) $ \ys ->
  (consConcatRule xs ys)

-- concatRule :: (Eq a) => [[a]]  -> Bool
-- concatRule xs = foldr (++) [] xs == concat xs

prop_concatRule :: Property
prop_concatRule =
  (forAll (arbitrary :: Gen [[Int]]))
  (concatRule)

-- takeLengthRule :: Int -> [a] -> Bool
-- takeLengthRule n xs = length (take n xs) == n

prop_takeLengthRule :: Property
prop_takeLengthRule =
  forAll (getPositive <$> (arbitrary :: Gen (Positive Int))) $ \n ->
  forAll (arbitrary :: Gen [Char]) $ \xs ->
  (takeLengthRule n xs)

prop_isomorphismRule :: Property
prop_isomorphismRule =
  forAll (arbitrary :: Gen Char)
  (isomorphismRule)

prop_squareIdentity :: Property
prop_squareIdentity = forAll
  (arbitrary :: Gen Double) $ \x ->
  (squareIdentity x) == id x

main :: IO ()
main = do
  putStrLn ""
  quickCheck prop_halfIdentity
  quickCheck prop_listOrdered
  quickCheck prop_plusAssociative
  quickCheck prop_plusCommutative
  quickCheck prop_multiplyAssociative
  quickCheck prop_multiplyCommutative
  quickCheck prop_quotRule
  quickCheck prop_divModRule
  quickCheck prop_powerCommutative
  quickCheck prop_powerAssociative
  quickCheck prop_reverseRule
  quickCheck prop_functionApplication
  quickCheck prop_functionComposition
  quickCheck prop_consConcatRule
  quickCheck prop_concatRule
  quickCheck prop_takeLengthRule
  quickCheck prop_isomorphismRule
  quickCheck prop_squareIdentity
  return ()
