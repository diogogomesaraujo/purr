{-# LANGUAGE DeriveDataTypeable #-}

module STG where
import Ast
import Data.Data (Data)

data Combinator = STGVar Identity
                | Combinator ::@ Combinator -- Application
                | S | K | I | B | C | Y
                | ADD | MUL | SUB | DIV
                | AND | OR | EQ | DIFF
                | GE | G | LE | L
                | STGConst Constant
                | STGIf
                | STGLet Identity
                deriving (Show, Eq, Data)
