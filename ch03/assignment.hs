-- Reading syntax
-- 1. For the following lines of code, read the syntax carefully and
-- decide if they are written correctly. Test them in your REPL after
-- you’ve decided to check your work. Correct as many as you can.
-- a) concat [[1, 2, 3], [4, 5, 6]]
concat [[1, 2, 3], [4, 5, 6]]
-- b) ++ [1, 2, 3] [4, 5, 6]
(++) [1, 2, 3] [4, 5, 6]
-- c) (++) "hello" " world"
-- d) ["hello" ++ " world]
["hello" ++ " world"]
-- e) 4 !! "hello"
"hello" !! 4
-- f ) (!!) "hello" 4
(!!) "hello" 4
-- g) take "4 lovely"
take 4 "lovely"
-- h) take 3 "awesome"
take 3 "awesome"

-- 2. Next we have two sets: the first set is lines of code and the other is
-- a set of results. Read the code and figure out which results came
-- from which lines of code. Be sure to test them in the REPL.
-- a) concat [[1 * 6], [2 * 6], [3 * 6]]
-- b) "rain" ++ drop 2 "elbow"
-- c) 10 * head [1, 2, 3]
-- d) (take 3 "Julie") ++ (tail "yes")
-- e) concat [tail [1, 2, 3],
-- tail [4, 5, 6],
-- tail [7, 8, 9]]
-- Can you match each of the previous expressions to one of these
-- results presented in a scrambled order?
-- a) "Jules"
-- b) [2,3,5,6,8,9]
-- c) "rainbow"
-- d) [6,12,18]
-- e) 10

-- a -> d
-- b -> c
-- c -> e
-- d -> a
-- e -> b


-- Building functions
-- 1. Given the list-manipulation functions mentioned in this chapter,
-- write functions that take the following inputs and return the
-- expected outputs. Do them directly in your REPL and use the
-- take and drop functions you’ve already seen.
-- Example
-- -- If you apply your function to this value:
-- "Hello World"
-- -- Your function should return:
-- "ello World"
-- The following would be a fine solution:
-- Prelude> drop 1 "Hello World"
-- "ello World"
-- Now write expressions to perform the following transformations,
-- just with the functions you’ve seen in this chapter. You do not
-- need to do anything clever here.
-- a) -- Given
-- "Curry is awesome"
-- -- Return
-- "Curry is awesome!"
f s = s ++ "!"

-- b) -- Given
-- "Curry is awesome!"
-- -- Return
-- "y"
g s = drop 4 (take 5 s)

-- c) -- Given
-- "Curry is awesome!"
-- -- Return
-- "awesome!"
h s = drop 9 s

-- 3. Write a function of type String -> Char which returns the third
-- character in a String. Remember to give the function a name
-- and apply it to a variable, not a specific String, so that it could
-- be reused for different String inputs, as demonstrated (feel free
-- to name the function something else. Be sure to fill in the type
-- signature and fill in the function definition after the equals sign):
-- thirdLetter ::
-- thirdLetter x =
-- -- If you apply your function to this value:
-- "Curry is awesome"
-- -- Your function should return
-- `r'
-- Note that programming languages conventionally start indexing
-- things by zero, so getting the zeroth index of a string will get you
-- the first letter. Accordingly, indexing with 3 will actually get you
-- the fourth. Keep this in mind as you write this function.
thirdLetter :: String -> Char
thirdLetter x = x !! 2

-- 4. Now change that function so the string input is always the same
-- and the variable represents the number of the letter you want to
-- return (you can use “Curry is awesome!” as your string input or
-- a different string if you prefer).
-- letterIndex :: Int -> Char
-- letterIndex x =

letterIndex :: Int -> Char
letterIndex x = "Curry is awesome!" !! x


-- 5. Using the take and drop functions we looked at above, see if you
-- can write a function called rvrs (an abbreviation of ‘reverse’ used
-- because there is a function called ‘reverse’ already in Prelude,
-- so if you call your function the same name, you’ll get an error
-- message). rvrs should take the string “Curry is awesome” and
-- return the result “awesome is Curry.” This may not be the most
-- lovely Haskell code you will ever write, but it is quite possible
-- using only what we’ve learned so far. First write it as a single
-- function in a source file. This doesn’t need, and shouldn’t, work
-- for reversing the words of any sentence. You’re expected only to
-- slice and dice this particular string with take and drop.
rvrs :: String -> String
rvrs s = (drop 9 s) ++ " " ++ (take 2 (drop 6 s)) ++ " " ++ (take 5 s)