module Parse where

import Test.HUnit
import Parser
import Lexer
import Ast

parseTests :: [Test]
parseTests = [testIncr]

testLexParse :: String -> Term -> Test
testLexParse str term = TestCase $ assertEqual
        ("lex and parse " ++ str)
        (parse $ alexScanTokens str)
        term

testIncr :: Test
testIncr =
    let incrStr  = "let incr := fn x -> x + 1 in incr 1"
        incrTerm  = Let "incr"
            (Lambda "x" $
                App (App (Prim (:+)) (Var "x")) (Const (CInt 1)))
            (App (Var "incr") (Const (CInt 1)))
    in testLexParse incrStr incrTerm
