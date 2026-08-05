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

type EitherTypSubst = Either Err (Typ, Subst)

-- | Function that types all untyped let, let rec and lambda terms recursively.
typeTerm :: Term -> Term
typeTerm (Lambda xs e)
    = TypedLambda xs (typeArgs xs []) e
typeTerm (Let x xs e1 e2)
    = TypedLet x xs (typeArgs xs []) e1 e2
typeTerm (LetRec x xs e1 e2)
    = TypedLetRec x xs (typeArgs xs []) e1 e2
typeTerm t = t

-- | Function that gives an arbitrary type to the arguments of a let, let rec or lambda term.
typeArgs :: [Identity] -> [Identity] -> DeclaredType
typeArgs [] _      = error "empty lambda"
typeArgs [_] ns
    = let n1 = newName ns
          n2 = newName (n1:ns) in
      DVar n1 ::-> DVar n2
typeArgs (_:xs') ns
    = let name = newName ns in
      DVar name ::-> typeArgs xs' (name:ns)

-- | Function that gives the absolute type of a complex type (ex: a -> b becomes b).
absTyp :: Typ -> Typ
absTyp (_ :-> t2) = absTyp t2
absTyp t = t

-- | Function that distinguishes primitive types from user created ones.
typFromId :: Identity -> Typ
typFromId "int"
    = TInt
typFromId "float"
    = TFloat
typFromId "bool"
    = TBool
typFromId t
    = TVar t

-- | Function that converts a user-declared type to one that can be checked.
typFromDeclaredType :: DeclaredType -> Typ
typFromDeclaredType (t1 ::-> t2)
    = typFromDeclaredType t1 :-> typFromDeclaredType t2
typFromDeclaredType (DVar t)
    = typFromId t

-- | Function that gives an unused name to an arbitrary type (used in typeOf).
newTVarName :: StaticEnv -> Identity
newTVarName env
    = findAvailableName $ combinations alphabet
        where findAvailableName (x:xs)
                  = if all (\(_, t) -> x `notElem` varsOf t) env
                        then x
                        else findAvailableName xs
              findAvailableName _ = error "impossible"
              varsOf (TVar v)    = [v]
              varsOf (t1 :-> t2) = nub $ varsOf t1 ++ varsOf t2
              varsOf (TList t)   = varsOf t
              varsOf _           = []

-- | Function that gives an unused name to an arbitrary type (used in typeTerm).s
newName :: [Identity] -> Identity
newName l
    = findAvailableName $ combinations alphabet
        where findAvailableName (x:xs)
                  = if x `notElem` l
                        then x
                        else findAvailableName xs
              findAvailableName _ = error "impossible"

-- | Function that gives all combinations of the elements in a list (percs of using Haskell xD).
combinations :: [a] -> [[a]]
combinations [] = [[]]
combinations (x:xs)
    =  combinations xs ++ map (x:)(combinations xs)

-- | Constant list of all letters in the alphabet.
alphabet :: [Char]
alphabet = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm'
           , 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z']
