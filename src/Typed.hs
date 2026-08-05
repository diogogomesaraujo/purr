module Typed where

import Ast
import Err
import Data.List


data Typ = TBool
         | TInt
         | TFloat
         | TList Typ
         | TVar Identity
         | Typ :-> Typ
         deriving (Show, Eq)

infixr 4 :->

type StaticEnv = [(Identity, Typ)]

type Subst = [(Typ, Typ)]

type EitherTyp = Either Err Typ

type EitherTypInfer = Either Err (Typ, Subst)

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
    = TVar t

typFromDeclaredType :: DeclaredType -> Typ
typFromDeclaredType (t1 ::-> t2)
    = typFromDeclaredType t1 :-> typFromDeclaredType t2
typFromDeclaredType (DVar t)
    = typFromId t

newTVarName :: StaticEnv -> Identity
newTVarName env
    = findAvailableName $ combinations alphabet
        where combinations [] = [[]]
              combinations (x:xs) =  combinations xs ++ map (x:)(combinations xs)
              alphabet
                  = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm'
                    , 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z']
              findAvailableName (x:xs)
                  = if all (\(_, t) -> x `notElem` varsOf t) env
                        then x
                        else findAvailableName xs
              findAvailableName _ = error "impossible"
              varsOf (TVar v)    = [v]
              varsOf (t1 :-> t2) = nub $ varsOf t1 ++ varsOf t2
              varsOf (TList t)   = varsOf t
              varsOf _           = []
