import Data.Char
import Data.List
import Data.List.Split

isSubsequenceOf' :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf' [] _ = True
isSubsequenceOf' _ [] = False
isSubsequenceOf' all@(x:xs) (y:ys) =
  if eq
  then (eq && isSubsequenceOf' xs ys)
  else isSubsequenceOf' all ys
  where eq = x == y

capitalizeWords :: String -> [(String, String)]
capitalizeWords xs = [(orig, capitalizeWord orig) | orig <- (words xs)]

capitalizeWord :: String -> String
capitalizeWord "" = ""
capitalizeWord (x:xs) = toUpper x:xs

capitalizeParagraph :: String -> String
capitalizeParagraph xs = concat $ intersperse stopSeq $ capitalizeWord <$> sentences xs
  where
    sentences xs = splitOn stopSeq xs
    stopSeq = ". "
