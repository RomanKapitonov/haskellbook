stops = "pbtdkg"
vowels = "aeiou"

nouns = ["cat", "sock", "ship", "hero", "monkey", "baby"]
verbs = ["rocks", "casts", "catches", "throws"]

stopVowelStop :: [(Char, Char, Char)]
stopVowelStop = [(s1, v, s2) | s1 <- stops, v <- vowels, s2 <- stops]

stopVowelStopStartWithP :: [(Char, Char, Char)]
stopVowelStopStartWithP = [(s1, v, s2) | s1 <- stops, v <- vowels, s2 <- stops, s1 == 'p']

nounVerbNout :: [(String, String, String)]
nounVerbNout = [(n1, v, n2) | n1 <- nouns, v <- verbs, n2 <- nouns]

-- Average length
seekritFunc :: String -> Int
seekritFunc x =
  div (sum (map length (words x)))
    (length (words x))

integralSeekritFunc :: Fractional a => String -> a
integralSeekritFunc x =
  fromIntegral(sum (map length (words x))) /
  fromIntegral(length (words x))

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr ((||) . f) False

myElem :: Eq a => a -> [a] -> Bool
myElem x = any (== x)

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr ((:) . f) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\x acc -> if f x then x:acc else acc) []

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr ((++) . f) []

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x:xs) = foldl compare x xs
  where compare acc x = case (f acc x) of
                        GT -> acc
                        _  -> x

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (x:xs) = foldl compare x xs
  where compare acc x = case (f acc x) of
                        LT -> acc
                        _  -> x

f x y z = h (subFunction x y z) where subFunction x y z = g x y z