module Main where

import Test.Hspec
import Test.QuickCheck

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where
    go  n   d count
      | n < d     = (count, n)
      | otherwise = go (n - d) d (count + 1)

trivialInt :: Gen Int
trivialInt = return 1

oneThroughThree :: Gen Int
oneThroughThree = elements [1, 2, 3]

oneThroughThree' :: Gen Int
oneThroughThree' = elements [1, 2, 2, 2, 2, 3]

genBool :: Gen Bool
genBool = choose (False, True)

genBool' :: Gen Bool
genBool' = elements [False, True]

genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]

genChar :: Gen Char
genChar = elements ['a'..'z']

genEither :: (Arbitrary a, Arbitrary b) => Gen (Either a b)
genEither = do
  a <- arbitrary
  b <- arbitrary
  elements [Left a, Right b]

-- equal probability
genMaybe :: Arbitrary a => Gen (Maybe a)
genMaybe = do
  a <- arbitrary
  elements [Nothing, Just a]

-- What QuickCheck actually does
-- so you get more Just values

genMaybe' :: Arbitrary a => Gen (Maybe a)
genMaybe' = do
  a <- arbitrary
  frequency [ (1, return Nothing)
            , (3, return (Just a))]

genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
genTuple = do
  a <- arbitrary
  b <- arbitrary
  return (a, b)

genThreeple :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (a, b, c)
genThreeple = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (a, b, c)

prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 1 > x

runQc :: IO ()
runQc = quickCheck prop_additionGreater

main :: IO ()
main = hspec $ do
  describe "Addition" $ do
    it "1 + 1 is greater than 1" $ do
      ((1 + 1) :: Integer) > (1 :: Integer) `shouldBe` True

    it "2 + 2 is equal to 4" $ do
      ((2 + 2) :: Integer) `shouldBe` 4

    it "15 divided by 3 is 5" $ do
      dividedBy (15 :: Integer) (3 :: Integer) `shouldBe` (5, 0)

    it "22 divided by 4 is 5 remainder 2" $ do
      dividedBy (22 :: Integer) (4 :: Integer) `shouldBe` (5, 2)

    it "x + 1 is always greater than x" $ do
      -- Randomly generated input (by QuickCheck)
      property $ \x -> x + 1 > (x :: Int)

recursion :: IO ()
recursion = hspec $ do
  describe "Multiplication" $ do
    it "5 times 6 is 30" $ do
      multRecursive (5 :: Integer) 6 `shouldBe` 30
    it "-2 times 2 is 4" $ do
      multRecursive ((-2) :: Integer) 2 `shouldBe` (-4)

multRecursive :: (Integral a) => a -> a -> a
multRecursive a b
    | a > 0 = b + multRecursive (a - 1) b
    | a < 0 = (negate b) + multRecursive (a + 1) b
    | otherwise = 0
