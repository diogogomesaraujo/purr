module STG where
import Ast

data Combinator = STGVar Identity
                | Combinator ::@ Combinator -- Application
                | S | K | I | B | C
                | Y
                | STGConst Constant
                | STGIf
                | STGLet Identity
                deriving Show
