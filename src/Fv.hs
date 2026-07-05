module Fv where

import Ast
import Data.List

contains :: Eq a => a -> [a] -> Bool
contains x xs
    = case find (==x) xs of
        Just _ -> True
        _      -> False

fv :: Term -> [Identity]
fv (Const _)         = []
fv (Prim _)          = []
fv (Var x)           = [x]
fv (Lambda x e)      = delete x (fv e)
fv (App e1 e2)       = (fv e1) `union` (fv e2)
fv (If e1 e2 e3)     = fv e1 `union` fv e2 `union` fv e3
fv (Let x xs e1 e2)  = fv e1 `union` filter (\e -> contains e (x:xs)) (fv e2)
fv (Fix e)           = fv e

isFv :: Identity -> Term -> Bool
isFv v t = any (== v) $ fv t
