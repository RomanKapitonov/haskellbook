import Data.Time

data DatabaseItem = DbString String | DbNumber Integer | DbDate UTCTime deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime
  (fromGregorian 1911 5 1)
  (secondsToDiffTime 34123)) , DbNumber 9001
  , DbString "Hello, world!" , DbDate (UTCTime
              (fromGregorian 1921 5 1)
              (secondsToDiffTime 34123))
  ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr match []
  where
    match a b = case a of
      (DbDate date) -> date : b
      _ -> b

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr match []
  where
    match a b = case a of
      (DbNumber number) -> number : b
      _ -> b

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate

sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb xs = (integralSum xs) / (integralLength xs)
  where integralSum = fromIntegral . sumDb
        integralLength = fromIntegral . length . filterDbNumber

fibs :: [Integer]
fibs = 1 : scanl (+) 1 fibs

fibsTwenty :: [Integer]
fibsTwenty = take 20 fibs

fibsLessThanHundred :: [Integer]
fibsLessThanHundred = takeWhile (<100) fibs

fact :: [Integer]
fact = scanl (*) 1 [2..]