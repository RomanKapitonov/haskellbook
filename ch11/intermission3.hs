{-# LANGUAGE GeneralizedNewtypeDeriving
  ,FlexibleInstances #-}

-- 1. data PugType = PugData
-- Cardinality: 1

-- 2.  data Airline = PapuAir
--                  | CatapultsR'Us
--                  | TakeYourChancesUnited
-- Cardinality: 3

-- 3. Cardinality: 65536

-- 4. No instance for (Bounded Integer)

-- 5. Int is bigger

-- 6. 2^8 == 256


-- 1. MakeExample :: Example
-- error: Data constructor not in scope: Example

-- 2. Prelude> :i Example
-- data Example = MakeExample  -- Defined at <interactive>:25:1
-- instance [safe] Show Example -- Defined at <interactive>:25:37

-- 3. data AnotherExample = AnotherMakeExample Int deriving Show
-- :t AnotherMakeExample
-- AnotherMakeExample :: Int -> AnotherExample

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

newtype Goats = Goats Int deriving (Eq, Show, TooMany)

-- 1.
instance TooMany (Int, String) where
  tooMany (n, _) = n > 42

-- 2. (Overlapping instance)
-- instance TooMany (Int, Int) where
--   tooMany (x, y) = (x + y) > 42

-- 3.
instance (Num a, TooMany a) => TooMany (a, a) where
  tooMany (x, y) = tooMany (x + y)
