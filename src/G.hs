{-# LANGUAGE DeriveDataTypeable #-}

module G where
import Ast
import Data.Data (Data)

data Combinator = STGVar Identity
                | Combinator ::@ Combinator -- Application
                | S | K | I | B | C | Y
                | ADD | MUL | SUB | DIV
                | AND | OR | EQ | DIFF
                | GE | G | LE | L
                | CAT
                | STGConst Constant
                | STGIf
                deriving (Show, Eq, Data)

cons :: Combinator
cons = STGVar "cons"
