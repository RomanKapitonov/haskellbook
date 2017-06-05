import Data.List
-- Does the following code typecheck? If not, why not?
data Person = Person Bool deriving Show
printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

-- 2. Does the following typecheck? If not, why not?
data Mood = Blah | Woot deriving Show

instance Eq Mood where
  (==) Blah Blah = True
  (==) Woot Woot = True
  (==) _ _ = False

settleDown x = if x == Woot then Blah else x

-- 3. If you were able to get settleDown to typecheck:
-- a) What values are acceptable inputs to that function?
-- Anything of Mood, either Blah or Woot
-- b) What will happen if you try to run settleDown 9? Why?
-- Type inference requires Int to be comparable with Mood
-- c) What will happen if you try to run Blah > Woot? Why?
-- There is not Ord instance to decide the order of type instances

-- 4. Does the following typecheck? If not, why not?
type Subject = String
type Verb = String
type Object = String

data Sentence = Sentence Subject Verb Object deriving (Eq, Show)

s1 = Sentence "dogs" "drool" -- This is partially applied constructor
s2 = Sentence "Julie" "loves" "dogs"

-- Given a datatype declaration, what can we do?
-- Given the following datatype definitions:

data Rocks = Rocks String deriving (Eq, Show, Ord)   -- +Ord
data Yeah = Yeah Bool deriving (Eq, Show, Ord)       -- +Ord
data Papu = Papu Rocks Yeah deriving (Eq, Show, Ord) -- +Ord

-- Which of the following will typecheck? For the ones that don’t type-check, why don’t they?
-- 1. phew = Papu "chases" True
phew = Papu (Rocks "chases") (Yeah True)

-- 2. truth = Papu (Rocks "chomskydoz") (Yeah True)
truth = Papu (Rocks "chomskydoz") (Yeah True)

-- 3. equalityForall :: Papu -> Papu -> Bool
-- equalityForall p p' = p == p'
equalityForall :: Papu -> Papu -> Bool
equalityForall p p' = p == p'

-- 4. comparePapus :: Papu -> Papu -> Bool
-- comparePapus p p' = p > p'
comparePapus :: Papu -> Papu -> Bool
comparePapus p p' = p > p'

chk :: Eq b => (a -> b) -> a -> b -> Bool
chk f a b = (f a == b)

arith :: Num b => (a -> b) -> Integer -> a -> b
arith f i a = fromIntegral(i) + (f a)
