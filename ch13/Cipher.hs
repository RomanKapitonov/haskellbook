module Cipher where

import Data.Char

type Key = String

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

zipWithOffset :: Key -> String -> [(Char, Maybe Int)]
zipWithOffset _ [] = []
zipWithOffset [] _ = []
zipWithOffset key@(x:xs) (y:ys)
  | isAsciiUpper y = (y, Just (offsetByChar x)) : zipWithOffset xs ys
  | otherwise      = (y, Nothing) : zipWithOffset key ys

offsetByChar :: Char -> Int
offsetByChar c
  | isAsciiUpper c = (ord c) - (ord 'A')
  | otherwise      = ((ord . toUpper) c) - (ord 'A')

shift :: Char -> Maybe Int -> Char
shift c Nothing = c
shift c (Just n)
  | ord c + n > ord 'Z' = shift c (Just (n - 26))
  | ord c + n < ord 'A' = shift c (Just (n + 26))
  | otherwise                = chr $ ord c + n

vignere :: Key -> String -> String
vignere key message = map (uncurry shift) (zipWithOffset (cycle upKey) upMessage)
  where
    upMessage = toUpper <$> message
    upKey = toUpper <$> key

unVignere :: Key -> String -> String
unVignere key message = map (uncurry shift) $ negateOffset <$> (zipWithOffset (cycle upKey) upMessage)
  where
    negateOffset (c, o) = (c, negate <$> o)
    upMessage = toUpper <$> message
    upKey = toUpper <$> key

main = do
  putStr "Enter your message: "
  message <- getLine
  putStr "Enter your key: "
  key <- getLine
  putStrLn $ vignere key message
