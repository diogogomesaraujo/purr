module Lex where

import Test.HUnit
import Lexer
import Token

lexTests :: [Test]
lexTests = [testComments]

testComments :: Test
testComments =
    let incrStr    = "let incr := fn x -> x + 1 in incr 1 -- just a comment"
        incrTokens = [ TokenLet, TokenVar "incr",
                       TokenAssign, TokenFn, TokenVar "x",
                       TokenArrow, TokenVar "x", TokenPlus,
                       TokenInt 1, TokenIn, TokenVar "incr",
                       TokenInt 1 ]
    in TestCase $ assertEqual
        ("lex: " ++ incrStr)
        incrTokens
        (alexScanTokens incrStr)
