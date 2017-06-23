module WordNumber where

import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord n = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"] !! fromIntegral(n)

digits :: Int -> [Int]
digits n
  | n `elem` [0..9] = [n]
  | otherwise = digits (n `div` 10) ++ [n `mod` 10]

wordNumber :: Int -> String
wordNumber n = (concat . intersperse "-") $ map digitToWord $ digits n
