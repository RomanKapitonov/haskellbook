-- 1.
-- id :: a -> a
-- :kind a
-- a :: *

-- 2.
-- r :: a -> f a
-- :kind a
-- a :: *
-- :kind f
-- f :: * -> *

-- example GHCi session above the functions
-- >>> notThe "the"
-- Nothing
-- >>> notThe "blahtheblah"
-- Just "blahtheblah"
-- >>> notThe "woot"
-- Just "woot"

vowels :: String
vowels = "aeiou"

notThe :: String -> Maybe String
notThe xs
  | xs == "the" = Nothing
  | otherwise   = Just xs

-- >>> replaceThe "the cow loves us" -- "a cow loves us"
replaceThe :: String -> String
replaceThe xss = unwords $ f <$> notThe <$> words xss
  where
    f Nothing = "a"
    f (Just xs) = xs

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = count . words
  where
    count (x:rest@(y:xs))
      | x == "the" && (head y) `elem` vowels = 1 + (count rest)
      | otherwise = 0 + (count rest)
    count (_:[]) = 0

countVowels :: String -> Integer
countVowels = fromIntegral . length . filter (`elem` vowels)

newtype Word' = Word' String deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord xs = if countVowels > countConsonants then Nothing else Just (Word' xs)
  where
    countVowels = fromIntegral . length $ filter (`elem` vowels) xs
    countConsonants = fromIntegral . length $ filter (`notElem` vowels) xs
