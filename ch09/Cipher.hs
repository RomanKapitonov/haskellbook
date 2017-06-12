module Cipher where

import Data.Char

caesar :: Int -> String -> String
caesar _ "" = ""
caesar n (x:xs) = ((chr . caesared) x) : caesar n xs
  where
    caesared x = ord x + (n `mod` 26)

unCaesar :: Int -> String -> String
unCaesar _ "" = ""
unCaesar n (x:xs) = ((chr . uncaesared) x) : unCaesar n xs
  where
    uncaesared x = ord x - (n `mod` 26)

main = do
  print $ "test" == (unCaesar 5 $ (caesar 5 "test"))
