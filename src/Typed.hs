module Typed where

import Ast
import Data.Set as S
import Data.List as L
import Data.Map as M

type AFunc = Identity
type AVar  = Identity

data Mono = TVar Identity
          | TApp AFunc [Mono]
          deriving (Show, Eq)

type Poly = (Set AVar, Mono)

type Aliases = Map AVar Mono

type Binding = (Identity, Poly)

type Env = [Binding]

-- generalize and instantiate

gen :: Env -> Mono -> Poly
gen env t
    = let alpha = fvMonoEnv t env in
      (alpha, t)

inst :: Set Identity -> Poly -> Mono
inst new_vars (alphas, t)
    = let vars = L.zip
            (S.toList new_vars)
            (S.toList alphas) in
      L.foldl (\acc (alpha, beta) ->
          replace alpha (TVar beta) acc) t vars


-- free variables

fvMono :: Mono -> Set Identity
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

-- canonicalize

canonicalize :: Map AVar Mono -> Mono -> Mono
canonicalize aliases (TVar a)
    = case M.lookup a aliases of
           Just t  -> canonicalize aliases t
           Nothing -> TVar a
canonicalize aliases (TApp f ts)
    = TApp f $ L.map
        (canonicalize aliases) ts

-- occurs
occurs :: Mono -> AVar -> Bool
occurs (TVar v) v' = v == v'
occurs (TApp f vs) v'
    = f == v' || L.foldl
                    (\acc v -> acc || occurs v v')
                    False vs

-- convert declared types

fromDeclaredType :: DeclaredType -> Mono
fromDeclaredType (DVar v)
    = TVar v
fromDeclaredType (DList t)
    = TApp "list" [fromDeclaredType t]
fromDeclaredType (t1 ::-> t2)
    = TApp "->" [ fromDeclaredType t1
                , fromDeclaredType t2 ]

-- arrow

arrow :: Mono -> Mono -> Mono
arrow t1 t2
    = TApp "->" [t1, t2]

mono :: Mono -> Poly
mono t = (S.empty, t)

-- environment

bind :: Env -> Identity -> Poly -> Env
bind env v t
    = env ++ [(v, t)]

getInEnv :: Env -> Identity -> Maybe Poly
getInEnv env v
    = L.lookup v env
