module STG where
import Ast

data Combinator = STGVar Identity
                | Combinator ::@ Combinator -- Application
                | S | K | I | B | C
                | Y Combinator
                | STGConst Constant
                | STGPrim Operation
                | STGIf Combinator Combinator Combinator
                | STGLet Identity Combinator Combinator
                | STGList [Combinator]
                deriving Show
