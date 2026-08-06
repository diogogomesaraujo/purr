module File where

import Eval
import Lexer
import Typed
import TypeOf
import Parser
import Unwind
import Compile

interpFile :: FilePath -> IO ()
interpFile path = do file <- readFile path
                     prog <- pure
                            $ parse
                            $ alexScanTokens
                            $ file
                     case typeOf [] $ typeTerm prog of
                        Right _ ->
                            case compileSTG prog of
                                Right p -> putStr $ (show $ eval $ unwind [p]) ++ "\n"
                                Left  e -> putStr $ show e
                        Left e  -> putStr $ show e
