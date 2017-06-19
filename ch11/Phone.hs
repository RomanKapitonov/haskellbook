module Phone where

import Data.Char
import Data.List
import Data.Maybe

type Digit = Char
type Presses = Int

type Values = String
data Button = Button Digit Values deriving (Show)
data DaPhone = DaPhone [Button] deriving (Show)

findButton :: DaPhone -> Char -> Maybe Button
findButton (DaPhone []) _ = Nothing
findButton (DaPhone (b:bs)) q
  | matching b = Just b
  | otherwise  = findButton (DaPhone bs) q
  where
    matching (Button _ vs) = (toLower q) `elem` vs

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps phone q = buildTuple $ fromJust $ findButton phone q
  where
    buildTuple (Button key values)
      | isUpper q = ('*', 1) : fetch
      | otherwise = fetch
      where
        fetch = [(key, fromJust $ (+1) <$> elemIndex (toLower q) values)]

phonePad :: DaPhone
phonePad = DaPhone [
    Button '1' "1"
  , Button '2' "abc2"
  , Button '3' "def3"
  , Button '4' "ghi4"
  , Button '5' "jkl5"
  , Button '6' "mno6"
  , Button '7' "pqrs7"
  , Button '8' "tuv8"
  , Button '9' "wxyz9"
  , Button '0' "+ 0"
  , Button '*' "^"
  , Button '#' ".,\n"
  ]

convo :: [String]
convo =
    ["Wanna play 20 questions",
     "Ya",
     "U 1st haha",
     "Lol ok. Have u ever tasted alcohol lol",
     "Lol ya",
     "Wow ur cool haha. Ur turn",
     "Ok. Do u think I am pretty Lol",
     "Lol ya",
     "Haha thanks just making sure rofl ur turn"]

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead phone xs = concat $ map (reverseTaps phonePad) xs

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps xs = sum $ map snd xs

main :: IO ()
main =
  print $ map (cellPhonesDead phonePad) convo






