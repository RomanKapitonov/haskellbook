-- 1. Given the following lines of code as they might appear in a
-- source file, how would you change them to use them directly in
-- the REPL?
-- half x = x / 2
-- square x = x * x

let half x = x / 2
let square x = x * x

-- 2. Write one function that can accept one argument and work for
-- all the following expressions. Be sure to name the function.
-- 3.14 * (5 * 5)
-- 3.14 * (10 * 10)
-- 3.14 * (2 * 2)
-- 3.14 * (4 * 4)

circleArea r = 3.14 * (r * r)

-- 3. Below are some pairs of functions that are alike except for parenthesization.
-- Read them carefully and decide if the parentheses change
-- the results of the function. Check your work in GHCi.
-- 1. a) 8 + 7 * 9
-- b) (8 + 7) * 9

-- Yes, the results are different

-- 2. a) perimeter x y = (x * 2) + (y * 2)
-- b) perimeter x y = x * 2 + y * 2

-- No, the results are the same

-- CHAPTER 2. HELLO, HASKELL! 63
-- 3. a) f x = x / 2 + 9
-- b) f x = x / (2 + 9)

-- Yes, the results are different

-- 4. The following code samples are broken and won’t compile. The first
-- two are as you might enter into the REPL; the third is from a source
-- file. Find the mistakes and fix them so that they will.
-- 1. let area x = 3. 14 * (x * x)
let arex x = 3.14 * (x * x)
-- 2. let double x = b * 2
let double x = x * 2
-- 3. x = 7
-- y = 10
-- f = x + y
x = 7
y = 10 -- indentation
f = x + y

-- 5. Rewrite the following let expressions into declarations with where
-- clauses:
-- 1. let x = 3; y = 1000 in x * 3 + y
(\x -> (\y -> x * 3 + y)) 3 1000
-- 2. let y = 10; x = 10 * 5 + y in x * 5
(\y -> (\x -> x * 5) (10 * 5 + y) ) 10
-- 3. let x = 7; y = negate x; z = y * 10 in z / x + y
(\x -> (\y -> (\z -> z / x + y) (y * 10) ) (negate x) ) 7

let z = 7
let y = z + 8
let x = y ^ 2
let waxOn = x * 5
