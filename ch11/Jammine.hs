module Jammin where

import Data.List

data Fruit =
    Peach
  | Plum
  | Apple
  | Blackberry deriving (Eq, Show, Ord)

data JamJars = Jam {
    fruit :: Fruit
  , amount :: Int
} deriving (Eq, Show, Ord)

-- Cardinality of JamJars: 4 * (Cardinality of Int)

row1 = Jam Peach 15
row2 = Jam Plum 5
row3 = Jam Apple 21
row4 = Jam Blackberry 12
row5 = Jam Peach 4
row6 = Jam Apple 9
allJam = [row1, row2, row3, row4, row5, row6]

jarsCount :: [JamJars] -> Int
jarsCount = (foldl (+) 0) . (map amount)

compareKind :: JamJars -> JamJars -> Ordering
compareKind (Jam f _) (Jam f' _) = compare f f'

compareAmount :: JamJars -> JamJars -> Ordering
compareAmount (Jam _ a) (Jam _ a') = compare a a'

mostRow :: [JamJars] -> JamJars
mostRow = maximumBy compareAmount

groupJam :: [JamJars] -> [[JamJars]]
groupJam = groupBy (\a b -> compareKind a b == EQ) . (sortBy compareKind)

data Fiction = Fiction deriving Show
data Nonfiction = Nonfiction deriving Show
data BookType = FictionBook Fiction
              | NonfictionBook Nonfiction
              deriving Show

type AuthorName = String
data Author = Author (AuthorName, BookType)

type AuthorName = String
data Author =
    Fiction AuthorName
  | Nonfiction AuthorName deriving (Eq, Show)











