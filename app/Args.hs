module Args where
import Options.Applicative

data Command = Repl
             | File FilePath

commandParser :: Parser Command
commandParser
    = maybe Repl File <$> optional
        (strArgument $ help "file path of the program to interpret")
