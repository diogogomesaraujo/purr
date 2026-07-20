module Main where

import Eval
import Lexer
import Parser
import Compile
import Options.Applicative

data CmdArgs = CmdArgs { filePath :: String }

cmdArgs :: Parser CmdArgs
cmdArgs = CmdArgs
            <$> strOption
                ( long "file"
                <> short 'f'
                <> help "interpret the purr program at the given file path" )

interpFile :: CmdArgs -> IO ()
interpFile args = do file <- readFile $ filePath args
                     prog <- pure
                            $ compileSTG
                            $ parse
                            $ alexScanTokens
                            $ file
                     case prog of
                        Right p -> putStr $ show $ eval p
                        Left  e -> putStr $ show e

main :: IO ()
main = interpFile =<< execParser opts
            where opts = info (cmdArgs <**> helper)
                            ( fullDesc
                            <> progDesc "interpreter environment for the purr programming language"
                            <> header "purr - a purrrely functional programming language" )
