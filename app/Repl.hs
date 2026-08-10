module Repl where
import System.Console.Terminal.Size
import Control.Monad

import Eval
import Lexer
import Typed
import TypeOf
import Parser
import Stdlib
import Unwind
import Compile
import Data.Text
import GHC.IO.Handle (hFlush)
import System.IO (stdout)
import G (maybeShowConst)

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

    text'   <- withStd text

    prog    <- pure
               $ typeTerm
               $ parse
               $ alexScanTokens
               $ text'

    recv;

    case typeOf predefinedEnv prog of
        Right (progTyp, _) -> do evaled <- pure
                                           $ compileSTG prog

                                 putStr $ Prelude.show progTyp;
                                 newline;

                                 recv;
                                 case evaled of
                                    Right p -> case eval $ unwind [p] of
                                                    [c] -> case maybeShowConst c of
                                                                Just cs -> putStr $ cs ++ "\n"
                                                                _       -> putStr "couldn't reach a final value"
                                                    _       -> putStr "couldn't reach a final value"
                                    Left  e -> putStr $ Prelude.show e
                                 newline
        Left e -> putStr $ Prelude.show e ++ "\n"

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
