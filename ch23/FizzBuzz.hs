module FizzBuzz where

import Control.Monad
import Control.Monad.Trans.State
import qualified Data.DList as DL

fizzBuzz :: Integer -> String
fizzBuzz n | n `mod` 15 == 0 = "FizzBuzz"
           | n `mod` 5 == 0  = "Fizz"
           | n `mod` 3 == 0  = "Buzz"
           | otherwise = show n

fizzbuzzList :: [Integer] -> DL.DList String
fizzbuzzList list =
  execState (mapM_ addResult list) DL.empty

addResult :: Integer -> State (DL.DList String) ()
addResult n = do
  xs <- get
  let result = fizzBuzz n
  put (DL.snoc xs result)

fizzbuzzFromTo :: Integer -> Integer -> DL.DList String
fizzbuzzFromTo x y
  | x == y = fizzbuzzList [x]
  | x < y  = fizzbuzzList [x..y]
  | x > y  = fizzbuzzList [y..x]

main :: IO ()
main =
  mapM_ putStrLn $ fizzbuzzFromTo 1 100
