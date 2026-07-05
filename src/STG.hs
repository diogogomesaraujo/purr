module STG where
import Ast

data STGOperation = (::+)
               | (::-)
               | (::/)
               | (::*)
               | (::&&)
               | (::||)
               | (::==)
               | (::!=)
               | (::<)
               | (::<=)
               | (::>)
               | (::>=)
               deriving Show

data Combinator = STGVar Identity
                | Combinator ::@ Combinator -- Application
                | S | K | I | B | C
                | Y Combinator
                | STGConst Constant
                | STGPrim STGOperation
                | STGIf Combinator Combinator Combinator
                | STGLet Identity Combinator Combinator
                deriving Show

opConvert :: Operation -> STGOperation
opConvert (:+) = (::+)
opConvert (:-) = (::-)
opConvert (:*) = (::*)
opConvert (:/) = (::/)
opConvert (:||) = (::||)
opConvert (:&&) = (::&&)
opConvert (:<) = (::<)
opConvert (:>) = (::>)
opConvert (:<=) = (::<=)
opConvert (:>=) = (::>=)
opConvert (:==) = (::==)
opConvert (:!=) = (::!=)
