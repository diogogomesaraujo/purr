module File where

import Eval
import Lexer
import Parser
import Unwind
import Compile

interpFile :: FilePath -> IO ()
interpFile path = do file <- readFile path
                     prog <- pure
                            $ compileSTG
                            $ parse
                            $ alexScanTokens
                            $ file
                     case prog of
                        Right p -> putStr $ show $ eval $ unwind [p]
                        Left  e -> putStr $ show e
