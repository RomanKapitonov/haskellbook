module LearnParsers where

import Text.Trifecta
import Text.Parser.Combinators

stop :: Parser a
stop = unexpected "stop"

one = char '1'
one' = one >> stop
one'' = one >> eof

-- read two characters, '1', and '2'
oneTwo = char '1' >> char '2'
-- read two characters, '1' and '2', then die
oneTwo' = oneTwo >> stop
oneTwo'' = oneTwo >> eof

oneTwoThreeS = string "123"
oneTwoS = string "12"
oneS = string "1"

oneTwoThreeC = char '1' >> char '2' >> char '3'
oneTwoC = char '1' >> char '2'
oneC = char '1'

testParse :: Parser Char -> IO ()
testParse p = print $ parseString p mempty "123"

testEof :: Parser () -> IO ()
testEof p = print $ parseString p mempty "123"

testParseS :: Parser String -> IO ()
testParseS p = print $ parseString p mempty "123"

pNL s = putStrLn ('\n' : s)

main = do
  pNL "stop:"
  testParse stop
  pNL "one:"
  testParse one
  pNL "one':"
  testParse one'
  pNL "oneTwo:"
  testParse oneTwo
  pNL "oneTwo':"
  testParse oneTwo'
  pNL "one EOF:"
  testEof one''
  pNL "oneTwo EOF:"
  testEof oneTwo''
  pNL "1\n12\n123\n"
  testParseS (choice [ oneTwoThreeS
                     , oneTwoS
                     , oneS ])
  pNL "1\n12\n123\n"
  testParse (choice [ oneTwoThreeC
                    , oneTwoC
                    , oneC])
