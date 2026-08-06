module Repl where
import System.Console.Terminal.Size
import Control.Monad

import Eval
import Lexer
import Typed
import TypeOf
import Parser
import Unwind
import Compile
import Data.Text
import GHC.IO.Handle (hFlush)
import System.IO (stdout)

repl :: IO ()
repl = do
    tH <- termHeight
    clear tH;

    welcome;

    replLoop;

replLoop :: IO ()
replLoop = do
    send;

    text    <- getText

    prog    <- pure
               $ typeTerm
               $ parse
               $ alexScanTokens
               $ text

    case typeOf [] prog of
        Right (progTyp, _) -> do evaled <- pure
                                           $ compileSTG prog

                                 recv;
                                 putStr $ Prelude.show progTyp ++ "\n";

                                 recv;
                                 _ <- case evaled of
                                    Right p -> putStr $ Prelude.show $ eval $ unwind [p]
                                    Left  e -> putStr $ Prelude.show e
                                 newline
        Left e -> putStr $ Prelude.show e

    replLoop

send :: IO ()
send = do putStr "> "; hFlush stdout

recv :: IO ()
recv = do putStr "< "; hFlush stdout

newline ::IO ()
newline = putStr "\n"

welcome :: IO ()
welcome = putStr "welcome to purr's repl! type your programs bellow.\n\n"

clear :: Int -> IO ()
clear n = replicateM_ n $ putStr "\n"

termHeight :: IO Int
termHeight = do
    maybeTermSize <- size
    pure $ case maybeTermSize of
               Just s -> height s
               _      -> error "couldn't get the terminal's size"

getText :: IO String
getText = getTextRec []
    where getTextRec t = do
            line <- getLine
            case Prelude.reverse $ unpack $ strip $ pack line of
                ';':';':rest -> pure $ t ++ " " ++ Prelude.reverse rest
                _  -> getTextRec $ t ++ "" ++ line
