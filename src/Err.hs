module Err where
import Token (Token)

-- | Type that defines the errors that can happen at compilation time.
data Err = Compiling String
         | Parsing [Token]
         deriving Show

-- | Function that shows a compile time error.
showErr :: Err -> String

showErr (Compiling e) = e

showErr (Parsing t) = "error parsing: " ++ show t
