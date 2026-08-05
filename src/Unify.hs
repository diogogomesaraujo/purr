module Unify where

import Typed
import Err
import Fv

unify :: Typ -> Typ -> Subst -> Either Err Subst
unify t1 t2 s
     = unify' (apply s t1) (apply s t2) s

unify' :: Typ -> Typ -> Subst -> Either Err Subst

unify' TBool TBool s
    = Right s

unify' TInt TInt s
    = Right s

unify' TFloat TFloat s
    = Right s

unify' (TVar v) t s
    | t == TVar v       = Right s
    | not $ isFvTyp t v = Right $ (TVar v, t):s
    | otherwise         = unifyErr (TVar v) t

unify' t (TVar v) s
    = unify (TVar v) t s

unify' (TList a) (TList b) s
    = unify a b s

unify' (t1 :-> t2) (t3 :-> t4) s
    = do s' <- unify t1 t3 s
         unify t2 t4 s'

unify' t1 t2 _
    = unifyErr t1 t2

unifyErr :: Typ -> Typ -> Either Err Subst
unifyErr t1 t2
    = Left
    $ Compiling
    $ "couldn't unify " ++ show t1 ++ " with " ++ show t2

apply :: Subst -> Typ -> Typ

apply s (TVar v)
    = case lookup (TVar v) s of
        Just t  -> apply s t
        Nothing -> TVar v

apply s (TList t)
    = TList $ apply s t

apply s (t1 :-> t2)
    = apply s t1 :-> apply s t2

apply _ t
    = t

applyToEnv :: StaticEnv -> Subst -> StaticEnv
applyToEnv env s
    = map (\(x, t) -> (x, apply s t)) env
