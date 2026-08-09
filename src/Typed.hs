module Typed where

import Ast
import Err
import Name
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

typeTerm :: Term -> Term
typeTerm t
    = let (t', _) = typeTerm' t [] in t'

-- | Function that types all untyped let, let rec and lambda terms recursively.
typeTerm' :: Term -> [Identity] -> (Term, [Identity])
typeTerm' (Lambda xs e) ns
    = let (tArgs, nArgs) = typeArgs xs ns
          (t, ns')       = typeTerm' e nArgs in
          (TypedLambda xs tArgs t, ns')
typeTerm' (Let x xs e1 e2) ns
    = let (tArgs, nArgs) = typeArgs xs ns
          (t1, ns1)       = typeTerm' e1 nArgs
          (t2, ns2)       = typeTerm' e2 ns1 in
      (TypedLet x xs tArgs t1 t2, ns2)
typeTerm' (LetRec x xs e1 e2) ns
    = let (tArgs, nArgs) = typeArgs xs ns
          (t1, ns1)       = typeTerm' e1 nArgs
          (t2, ns2)       = typeTerm' e2 ns1 in
      (TypedLetRec x xs tArgs t1 t2, ns2)
typeTerm' (TypedLambda xs t e) ns
    = let (te, ns') = typeTerm' e ns in
      (TypedLambda xs t te, ns')
typeTerm' (TypedLet x xs t e1 e2) ns
    = let (t1, ns1)       = typeTerm' e1 ns
          (t2, ns2)       = typeTerm' e2 ns1 in
      (TypedLet x xs t t1 t2, ns2)
typeTerm' (TypedLetRec x xs t e1 e2) ns
    = let (t1, ns1)       = typeTerm' e1 ns
          (t2, ns2)       = typeTerm' e2 ns1 in
      (TypedLetRec x xs t t1 t2, ns2)
typeTerm' (If e1 e2 e3) ns
    = let (t1, ns1)       = typeTerm' e1 ns
          (t2, ns2)       = typeTerm' e2 ns1
          (t3, ns3)       = typeTerm' e3 ns2 in
      (If t1 t2 t3, ns3)
typeTerm' (Fix e) ns
    = let (t', ns') = typeTerm' e ns in
      (Fix t', ns')
typeTerm' (e1 :@ e2) ns
    = let (t1, ns1)       = typeTerm' e1 ns
          (t2, ns2)       = typeTerm' e2 ns1 in
      (t1 :@ t2, ns2)
typeTerm' (Const (CList [])) ns
    = (Const $ CList [], ns)
typeTerm' (Const (CList es)) ns
    = let (t, ns') = typeList es ns in
      (Const $ CList t, ns')
typeTerm' e ns = (e, ns)

typeList :: [Term] -> [Identity] -> ([Term], [Identity])
typeList (e:es) ns
    = let (t, ns')  = typeTerm' e ns
          (ts, nss) = typeList es ns' in
      (t:ts, nss)
typeList [] ns
    = ([], ns)

-- | Function that gives an arbitrary type to the arguments of a let, let rec or lambda term.
typeArgs :: [Identity] -> [Identity] -> (DeclaredType, [Identity])
typeArgs [] ns
    = let n = newName ns in (DVar $ n, n:ns)
typeArgs [_] ns
    = let n1 = newName ns
          n2 = newName (n1:ns) in
      (DVar n1 ::-> DVar n2, n1:n2:ns)
typeArgs (_:xs') ns
    = let n         = newName ns
          (n', ns') = typeArgs xs' (n:ns) in
      (DVar n ::-> n', ns')

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
typFromDeclaredType (DList t)
    = TList $ typFromDeclaredType t

-- | Function that gives an unused name to an arbitrary type (used in typeOf).
newTVarName :: StaticEnv -> Identity
newTVarName env
    = findAvailableName $ combinations alphabet \\ [""]
        where findAvailableName (x:xs)
                  = if all (\(_, t) -> x `notElem` varsOf t) env
                        then x
                        else findAvailableName xs
              findAvailableName _ = error "impossible"
              varsOf (TVar v)    = [v]
              varsOf (t1 :-> t2) = nub $ varsOf t1 ++ varsOf t2
              varsOf (TList t)   = varsOf t
              varsOf _           = []

showTyp :: Typ -> String
showTyp (TVar v)    = v
showTyp TBool       = "bool"
showTyp TFloat      = "float"
showTyp TInt        = "int"
showTyp (TList t)   = "[" ++ showTyp t ++ "]"
showTyp (t1 :-> t2) = showTyp t1 ++ " -> " ++ showTyp t2

showArgs :: Char -> (a -> String) -> [a] -> String
showArgs sep f ts =
    foldl (\acc t' ->
                f t' ++ show sep ++ acc)
    "" ts
