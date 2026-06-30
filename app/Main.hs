module Main (main) where

import Parser
import Lexer

main :: IO ()
main = do
  parsed <- parse $ lexer "let x := 3 + 2 in x"
  putStr $ show parsed
