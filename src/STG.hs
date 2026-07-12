module STG where
import Ast

data Combinator = STGVar Identity
                | Combinator ::@ Combinator -- Application
                | S | K | I | B | C
                | Y Combinator
                | STGConst Constant
                | STGIf Combinator Combinator Combinator
                | STGLet Identity Combinator Combinator
                deriving Show

cons :: Combinator -> Combinator -> Combinator
cons x y = (STGVar "cons" ::@ x) ::@ y

nil :: Combinator
nil = STGVar "nil"
