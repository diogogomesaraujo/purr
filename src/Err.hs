module Err where
import Token (Token)

data Err = Compiling String
         | Parsing [Token]
         deriving Show

showErr :: Err -> String

showErr (Compiling e) = e

showErr (Parsing t) = "error parsing: " ++ show t
