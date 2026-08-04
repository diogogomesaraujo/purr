module Typed where

import Ast

data Typ = TBool
         | TInt
         | TFloat
         | TList Typ
         | Typ :-> Typ
         deriving (Show, Eq)

infixr 4 :->

absTyp :: Typ -> Typ
absTyp (_ :-> t2) = absTyp t2
absTyp t = t

typFromId :: Identity -> Typ
typFromId "int"
    = TInt
typFromId "float"
    = TFloat
typFromId "bool"
    = TBool
typFromId t
    = error $ "the type "
            ++ t
            ++ " is not recognized by the compiler"

typFromDeclaredType :: DeclaredType -> Typ
typFromDeclaredType (t1 ::-> t2)
    = typFromDeclaredType t1 :-> typFromDeclaredType t2
typFromDeclaredType (TVar t)
    = typFromId t
