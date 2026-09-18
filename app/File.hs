module File where

import G
import J
import Eval
import Lexer
import Parser
import Unwind
import Compile
import Stdlib (withStd)

interpFile :: FilePath -> IO ()
interpFile path = do file  <- readFile path
                     file' <- withStd file
                     prog  <- pure
                             $ parse
                             $ alexScanTokens
                             $ file'
                     case infer newJ prog of
                        Right _ ->
                            case compileSTG prog of
                                Right p ->
                                    case eval $ unwind [p] of
                                        [c] -> case maybeShowConst c of
                                                    Just cs -> putStr $ cs ++ "\n"
                                                    _       -> putStr "couldn't reach a final value \n"
                                        _       -> putStr "couldn't reach a final value \n"
                                Left  e -> putStr $ show e ++ "\n"
                        Left e  -> putStr $ show e ++ "\n"
