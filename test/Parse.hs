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
            (((Prim (:+)) :@ (Var "x")) :@ (Const (CInt 1)))
            ((Var "incr") :@ (Const (CInt 1)))
    in testLexParse incrStr incrTerm

testParseLetOp :: Test
testParseLetOp =
    let opStr  = "let (**) : x y = x * y in 2 ** 3"
        opTerm = Let "**" ["x", "y"] (((Prim (:*)) :@ (Var "x")) :@ (Var "y")) (((Prim (Custom "**")) :@ (Const (CInt 2))) :@ (Const (CInt 3)))
    in testLexParse opStr opTerm

testParseLetRecOp :: Test
testParseLetRecOp =
    let opStr  = "let rec (^) : x y = if y == 0 then 1 else (if y > 1 then (x * x) ^ (y - 1) else x in 2 ^ 3)"
        opTerm = Let "**" ["x", "y"] (((Prim (:*)) :@ (Var "x")) :@ (Var "y")) (((Prim (Custom "**")) :@ (Const (CInt 2))) :@ (Const (CInt 3)))
    in testLexParse opStr opTerm

testParseFun :: Test
testParseFun =
    let funStr  = "(fn x -> fn y -> x + y) 2 3"
        funTerm = (Lambda "x" (Lambda "y" ((Prim (:+) :@ Var "x") :@ Var "y"))) :@ Const (CInt 2) :@ Const (CInt 3)
    in testLexParse funStr funTerm
