module File where

import J
import Eval
import Lexer
import Parser
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
                            case eval prog of
                                 Right v -> putStr $ showValue v ++ "\n"
                                 Left  e -> putStr $ Prelude.show e ++ "\n"
                        Left e  -> putStr $ show e ++ "\n"
