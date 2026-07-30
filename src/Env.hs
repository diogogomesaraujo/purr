module Env where

import Ast
import Typed

type StaticEnv = [(Identity, Typ)]

typeOf :: StaticEnv -> Term -> Maybe Typ

typeOf _ (Const (CBool _))
    = Just TBool

typeOf _ (Const (CInt _))
    = Just TInt

typeOf _ (Const (CFloat _))
    = Just TFloat

typeOf env (Var v)
    = lookup v env

typeOf env (Let x _ e1 e2)
    = do t1   <- typeOf env e1
         env' <- pure $ (x, t1):env
         typeOf env' e2

typeOf env (LetRec x _ e1 e2)
    = do t1   <- typeOf env e1
         env' <- pure $ (x, t1):env
         typeOf env' e2

typeOf env (If e1 e2 e3)
    = if typeOf env e1 == Just TBool
        then let t2 = typeOf env e2
        in if t2 == typeOf env e3
            then t2
            else Nothing
        else Nothing

typeOf env (Fix e1)
    = case typeOf env e1 of
           Just (_ :-> e1') -> Just e1'
           _                -> Nothing

typeOf env (e1 :@ e2)
    = case typeOf env e1 of
           Just (t1 :-> t2) ->
                let t2' = typeOf env e2
                in if Just t1 == t2'
                    then Just t2
                    else Nothing
           _               -> Nothing

typeOf _ _ = undefined
