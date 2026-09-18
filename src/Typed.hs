module Typed where

import Ast
import Data.Set as S
import Data.List as L
import Data.Map as M

type AFunc = Identity
type AVar  = Identity

data Mono = TVar   Identity
          | TConst Identity
          | TApp   AFunc [Mono]
          deriving Eq

instance Show Mono where
    show (TVar v)   = v
    show (TConst c) = c
    show (TApp "->" (x:y:[]))
        = show x ++ " -> " ++ show y
    show (TApp "list" (x:[]))
        = "[" ++ show x ++ "]"
    show (TApp f xs) = show f
                       ++ " "
                       ++ show xs

type Poly = (Set AVar, Mono)

showPoly :: Poly -> String
showPoly (sxs, t)
    | not $ S.null sxs = let xs = S.toList sxs in
                       "forall " ++ showXs xs
                       ++ ". "   ++ show t
    | otherwise = show t
    where showXs (x:[]) = x
          showXs [] = ""
          showXs (x:xs')
             = x ++ ", " ++ showXs xs'

type Aliases = Map AVar Mono

type Binding = (Identity, Poly)

type Env = [Binding]

-- primitives
prims :: Set Identity
prims = S.fromList ["int", "float", "bool"]

-- generalize and instantiate

gen :: Env -> Mono -> Poly
gen env t
    = let alpha = fvMonoEnv t env in
      (alpha S.\\ prims, t)

inst :: Set Identity -> Poly -> Mono
inst new_vars (alphas, t)
    = let vars = L.zip
            (S.toList alphas)
            (S.toList new_vars) in
      L.foldl (\acc (alpha, beta) ->
          replace alpha (TVar beta) acc) t vars


-- free variables

fvMono :: Mono -> Set Identity
fvMono (TConst _) = S.empty
fvMono (TVar v)
    = S.singleton v
fvMono (TApp _ ts)
    = L.foldl (\acc t ->
        S.union (fvMono t) acc) S.empty ts

fvPoly :: Poly -> Set Identity
fvPoly (vs, t) =
    fvMono t S.\\ vs

fvEnv :: Env -> Set Identity
fvEnv env
    = L.foldl (\acc (_, t) ->
        fvPoly t `S.union` acc )
            S.empty env

fvMonoEnv :: Mono -> Env -> Set Identity
fvMonoEnv t env
    = fvMono t S.\\ fvEnv env

-- replace

replace :: Identity -> Mono -> Mono -> Mono
replace a t (TVar v)
    | v == a    = t
    | otherwise = TVar v
replace a t (TApp f ts)
    = TApp f $ L.map (replace a t) ts
replace _ _ (TConst c)
    = TConst c

-- canonicalize

canonicalize :: Map AVar Mono -> Mono -> Mono
canonicalize aliases (TVar a)
    = case M.lookup a aliases of
           Just t  -> canonicalize aliases t
           Nothing -> TVar a
canonicalize aliases (TApp f ts)
    = TApp f $ L.map
        (canonicalize aliases) ts
canonicalize _ (TConst c) = TConst c

-- occurs
occurs :: Mono -> AVar -> Bool
occurs (TConst _) _ = False
occurs (TVar v) v' = v == v'
occurs (TApp _ vs) v'
    = any (`occurs` v') vs

-- convert declared types

fromDeclaredType :: DeclaredType -> Mono
fromDeclaredType (DVar v)
    = case S.member v prims of
           True  -> TConst v
           False -> TVar v
fromDeclaredType (DList t)
    = TApp "list" [fromDeclaredType t]
fromDeclaredType (t1 ::-> t2)
    = TApp "->" [ fromDeclaredType t1
                , fromDeclaredType t2 ]

-- arrow

arrow :: Mono -> Mono -> Mono
arrow t1 t2
    = TApp "->" [t1, t2]

(--->) :: Mono -> Mono -> Mono
a ---> b = arrow a b

infixr 5 --->

mono :: Mono -> Poly
mono t = (S.empty, t)

-- environment

bind :: Env -> Identity -> Poly -> Env
bind env v t
    = env ++ [(v, t)]

getInEnv :: Env -> Identity -> Maybe Poly
getInEnv env v
    = L.lookup v env

-- standard environment

tint :: Mono
tint = TConst "int"

tfloat :: Mono
tfloat = TConst "float"

tbool :: Mono
tbool = TConst "bool"

tarith :: Identity -> Binding
tarith p = (p, (S.singleton idA, ta ---> ta ---> ta))

tcompare :: Identity -> Binding
tcompare p = (p, (S.singleton idA, ta ---> ta ---> tbool))

tandor :: Identity -> Binding
tandor p = (p, (S.empty, tbool ---> tbool ---> tbool))

idA :: Identity
idA = "_a"

ta :: Mono
ta = TVar idA

tif :: Binding
tif = ("if", (S.singleton idA, tbool ---> ta ---> ta ---> ta))

tfix :: Binding
tfix = ("fix", (S.singleton idA, (ta ---> ta) ---> ta))

tunion :: Binding
tunion = (":", (S.singleton idA, ta ---> TApp "list" [ta] ---> TApp "list" [ta]))

thead :: Binding
thead = ("head", (S.singleton idA, TApp "list" [ta] ---> ta))

ttail :: Binding
ttail = ("tail", (S.singleton idA, TApp "list" [ta] ---> TApp "list" [ta]))

stdEnv :: Env
stdEnv = [ tif, tfix, tunion, thead, ttail,
           tandor "||", tandor "&&", tcompare "<"
           , tcompare "<=", tcompare ">",  tcompare ">="
           , tcompare "==" , tcompare "!=", tarith "+"
           , tarith "-", tarith "*", tarith "/" ]
