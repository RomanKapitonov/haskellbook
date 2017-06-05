-- 1. What is the type of [[True, False], [True, True], [False,
-- True]]?
-- a) Bool
-- b) mostly True
-- c) [a]
-- d) [[Bool]] - This one

-- 2. Which of the following has the same type as [[True, False], [True, True], [False, True]]?
-- a) [(True, False), (True, True), (False, True)]
-- b) [[3 == 3], [6 > 5], [3 < 4]] - This one
-- c) [3 == 3, 6 > 5, 3 < 4]
-- d) ["Bool", "more Bool", "Booly Bool!"]

-- 3. For the following function
-- func :: [a] -> [a] -> [a] func x y = x ++ y
-- which of the following is true?
-- a) xandymustbeofthesametype
-- b) x and y must both be lists
-- c) ifxisaStringthenymustbeaString
-- d) all of the above - This one

-- 4. For the func code above, which is a valid application of func to both of its arguments?
-- a) func "Hello World"
-- b) func "Hello" "World" - This one
-- c) func [1, 2, 3] "a, b, c"
-- d) func ["Hello", "World"]


-- Given the following definitions, tell us what value results from further applications.
cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String
appedCatty = cattyConny "woops"

frappe :: String -> String
frappe = flippy "haha"

-- 1. What is the value of appedCatty "woohoo!" ? Try to determine
-- the answer for yourself, then test in the REPL.
-- woops mrow woohoo!

-- 2. frappe "1"
-- "1 mrow haha"

-- 3. frappe (appedCatty "2")
-- "woops mrow 2 mrow haha"

-- 4. appedCatty (frappe "blue")
-- "woops mrow blue mrow haha"

-- 5. cattyConny (frappe "pink") (cattyConny "green" (appedCatty "blue"))
-- pink mrow haha mrow green mrow woops mrow blue

-- 6. cattyConny (flippy "Pugs" "are") "awesome"
-- are mrow Pugs mrow awesome

-- 1. Write out the steps for reducing dividedBy 15 2 to its final answer according to the Haskell code.
-- dividedBy 15 2 =
--   go 15 2 0
--   | 15 < 2 = ...
--   -- false, skip this branch
--   | otherwise = go (15 - 2) 2 (0 + 1)
--   -- otherwise is literally the value True
--   -- so if first branch fails, this always succeeds
--     go 13 2 1
--     -- 13 isn't < 2, so the otherwise branch go (13 - 2) 2 (1 + 1)
--     -- n == 11, d == 2, count == 2
--       go 11 2 2
--         go (11 - 2) 2 (2 + 1)
--         -- 9 isn't < 2, so the otherwise branch
--         -- n == 4, d == 2, count == 3
--           go 9 2 3
--             go (9 - 2) 2 (3 + 1)
--             -- 9 isn't < 2, so the otherwise branch
--             -- 7 == 2, d == 2, count == 4
--               go 7 2 4
--                 go (7 - 2) 2 (4 + 1)
--                 -- 7 isn't < 2, so the otherwise branch
--                 --n == 5, d == 2, count == 5
--                   go 5 2 5
--                     go (5 - 2) 2 (5 + 1)
--                     -- 5 isn't < 2, so the otherwise branch
--                     --n == 5, d == 2, count == 5
--                       go 3 2 6
--                         go (3 - 2) 2 (6 + 1)
--                         -- 3 isn't < 2, so the otherwise branch
--                         --n == 1, d == 2, count == 5
--                           go 1 2 7
--                           -- the n < d branch is finally evaluated -- because 1 < 2 is true
--                           -- n == 1, d == 2, count == 5 | 0 < 2= (7, 0)


-- 2. Write a function that recursively sums all numbers from 1 to n, n being the argument.
-- So that if n was 5,you’d add 1 + 2 + 3 + 4 + 5 to get 15. The type should be(Eq a, Num a) => a -> a.
s :: (Eq a, Num a) => a -> a
s n
  | n == 0 = n
  | otherwise = n + s(n - 1)

-- 3. Write a function that multiplies two integral numbers using recursive summation.
-- The type should be (Integral a) => a -> a -> a.
m :: (Integral a) => a -> a -> a
m a b
  | b == 0 = 0
  | otherwise = a + m a (b - 1)

-- Fixing dividedBy
-- Our dividedBy function wasn’t quite ideal. For one thing.
-- It was a partial function and doesn’t return a result (bottom) when given a divisor that is 0 or less.

data DividedResult = Result Integer | DividedByZero deriving Show

dividedBy :: Integral a => a -> a -> DividedResult
dividedBy num denom = go num denom 0
  where go n d count
         | d == 0 = DividedByZero
         | n < d = (Result count)
         | otherwise = go (n - d) d (count + 1)


mc91 :: Integer -> Integer
mc91 n
  | n > 100 = n - 10
  | otherwise = (mc91 . mc91) $ n + 11

-- map mc91 [95..110]

numWords = 

wordNumber :: Integer -> String
wordNumber n
  | n `elem` [0..9] = numWords !! idx
  | otherwise = wordNumber divRes ++ "-" ++ (numWords !! modRes)
  where
    modRes = fromIntegral(n `mod` 10)
    divRes = fromIntegral(n `div` 10)
    idx = fromIntegral(n)