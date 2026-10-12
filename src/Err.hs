module Err where
import Token (Token)

-- | Type that defines the errors that can happen at compilation time.
data Err = Compiling String
         | Parsing [Token]
         | Evaluating String
         deriving (Show, Eq)

-- | Function that shows a compile time error.
showErr :: Err -> String
showErr (Compiling e)  = "error compiling: "  ++ e
showErr (Parsing t)    = "error parsing: "    ++ show t
showErr (Evaluating e) = "error evaluating: " ++ e
