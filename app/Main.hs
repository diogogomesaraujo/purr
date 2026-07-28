module Main where

import File
import Args
import Repl
import Options.Applicative

exec :: Command -> IO ()
exec cmd =
    case cmd of
        Repl      -> repl
        File path -> interpFile path

main :: IO ()
main = exec =<< execParser opts
            where opts = info (commandParser <**> helper)
                            ( fullDesc
                            <> progDesc "interpreter environment for the purr programming language"
                            <> header "purr - a purrrely functional programming language" )
