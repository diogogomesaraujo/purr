module G where

import Ast

data Combinator = STGVar Identity
                | Combinator ::@ Combinator -- Application
                | S | K | I | B | C | Y
                | ADD | MUL | SUB | DIV
                | AND | OR | EQ | DIFF
                | GE | G | LE | L
                | CONS
                | STGConst Constant
                | STGIf
                deriving (Show, Eq)

cons :: Combinator
cons = STGVar "cons"
