{-# LANGUAGE QuasiQuotes #-}

module AltParsing where

import Control.Applicative
import Text.Trifecta
import Data.Ratio ((%))
import Text.RawString.QQ

type NumberOrString = Either Integer String
type RationalOrInteger = Either Rational Integer

a = "blah"
b = "123"
c = "123blah789"

parseNos :: Parser NumberOrString
parseNos = skipMany (oneOf "\n") >> (Left <$> integer) <|> (Right <$> some letter)

parseFraction :: Parser Rational
parseFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  case denominator of
    0 -> fail "Denominator cannot be zero"
    _ -> return (numerator % denominator)

parseDecimal :: Parser Integer
parseDecimal = do
  d <- decimal
  c <- anyChar
  case c of
    '\n' -> return d
    _ -> fail "Can no parse"

eitherOr :: String
eitherOr = [r|
123
abc
456
def|]

eitherFractional = [r|
123
456
1/2
5/7
|]

fractionOrDecimal :: Parser RationalOrInteger
fractionOrDecimal = skipMany (oneOf "\n") >> (Left <$> try parseFraction) <|> (Right <$> parseDecimal)

main :: IO ()
main = do
  print $ parseString (some letter) mempty a
  print $ parseString integer mempty b
  print $ parseString parseNos mempty a
  print $ parseString parseNos mempty b
  print $ parseString (many parseNos) mempty c
  print $ parseString (some parseNos) mempty c
  print $ parseString (some (token parseNos)) mempty eitherOr
  print $ parseString (some (token fractionOrDecimal)) mempty eitherFractional
