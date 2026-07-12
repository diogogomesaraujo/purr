module Parse where

import Test.HUnit
import Parser
import Lexer
import Ast

parseTests :: [Test]
parseTests = [testParseLet, testParseLetOp]

testLexParse :: String -> Term -> Test
testLexParse str term = TestCase $ assertEqual
        ("lex and parse " ++ str)
        (parse $ alexScanTokens str)
        term

testParseLet :: Test
testParseLet =
    let incrStr  = "let incr : x = x + 1 in incr 1"
        incrTerm  = Let "incr" ["x"]
            (App (App (Prim (:+)) (Var "x")) (Const (CInt 1)))
            (App (Var "incr") (Const (CInt 1)))
    in testLexParse incrStr incrTerm

testParseLetOp :: Test
testParseLetOp =
    let opStr  = "let op ** : x y = x * y in 2 ** 3"
        opTerm = LetOp "**" "x" "y" (App (App (Prim (:*)) (Var "x")) (Var "y")) (App (App (Prim (Custom "**")) (Const (CInt 2))) (Const (CInt 3)))
    in testLexParse opStr opTerm
