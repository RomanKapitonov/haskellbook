import Control.Monad
import System.Exit (exitSuccess)
import Data.Char (toLower
                , isLetter)

palindrome :: IO ()
palindrome = forever $ do
  line1 <- process <$> getLine
  case (line1 == reverse line1) of
    True -> putStrLn "It's a palindrome!"
    False -> do
      putStrLn "Nope!"
      exitSuccess
  where
    process = filter isLetter . map toLower

testString :: String
testString = "Madam I'm Adam"