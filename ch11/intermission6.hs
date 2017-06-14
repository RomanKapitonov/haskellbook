import Data.List

data GuessWhat =
  Chickenbutt deriving (Eq, Show)
data Id a =
    MkId a deriving (Eq, Show)
data Product a b =
  Product a b deriving (Eq, Show)
data Sum a b = First a | Second b deriving (Eq, Show)
data RecordProduct a b = RecordProduct {
    pfirst :: a
  , psecond :: b
} deriving (Eq, Show)

data OperatingSystem =
    GnuPlusLinux
  | OpenBSDPlusNevermindJustBSDStill
  | Mac
  | Windows
  deriving (Eq, Show)

data ProgrammingLanguage =
    Haskell
  | Agda
  | Idris
  | PureScript
  deriving (Eq, Show)

data Programmer = Programmer {
    os :: OperatingSystem
  , lang :: ProgrammingLanguage
} deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
  [ GnuPlusLinux
  , OpenBSDPlusNevermindJustBSDStill
  , Mac
  , Windows
  ]

allLanguages :: [ProgrammingLanguage]
allLanguages = [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers = [Programmer os lang | os <- allOperatingSystems, lang <- allLanguages ]

-- convert1 :: Quantum -> Bool
-- convert1 Yes  = True
-- convert1 No   = True
-- convert1 Both = True

-- convert2 :: Quantum -> Bool
-- convert2 Yes  = True
-- convert2 No   = True
-- convert2 Both = False

-- convert3 :: Quantum -> Bool
-- convert3 Yes  = True
-- convert3 No   = False
-- convert3 Both = True

-- convert4 :: Quantum -> Bool
-- convert4 Yes  = False
-- convert4 No   = True
-- convert4 Both = True

-- convert5 :: Quantum -> Bool
-- convert5 Yes  = False
-- convert5 No   = False
-- convert5 Both = True

-- convert6 :: Quantum -> Bool
-- convert6 Yes  = True
-- convert6 No   = False
-- convert6 Both = False

-- convert7 :: Quantum -> Bool
-- convert7 Yes  = False
-- convert7 No   = True
-- convert7 Both = False

-- convert8 :: Quantum -> Bool
-- convert8 Yes  = False
-- convert8 No   = False
-- convert8 Both = False

-- data Quad =
--     One
--   | Two
--   | Three
--   | Four
--   deriving (Eq, Show)

-- how many different forms can this take?

-- 1. eQuad :: Either Quad Quad
-- 4 + 4

-- 2. prodQuad :: (Quad, Quad)
-- 4 * 4

-- 3. funcQuad :: Quad -> Quad
-- 4 ^ 4

-- 4. prodTBool :: (Bool, Bool, Bool)
-- 2 * 2 * 2

-- 5. gTwo :: Bool -> Bool -> Bool
-- 2 ^ 2 ^ 2

-- 6. fTwo :: Bool -> Quad -> Quad
-- (2 ^ 4) ^ 4



