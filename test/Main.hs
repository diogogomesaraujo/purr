module Main where

import Test.HUnit
import Parse
import Lex

tests :: [Test]
tests = parseTests ++ lexTests

main :: IO ()
main = do
    _ <- runTestTT $ TestList tests
    return ()
