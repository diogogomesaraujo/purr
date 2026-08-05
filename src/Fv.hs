module Fv where

import Ast
import Data.List
import Typed
import G

-- Function that checks if a value exists inside a list.
contains :: Eq a => a -> [a] -> Bool
contains x xs
    = case find (==x) xs of
        Just _ -> True
        _      -> False

-- | Function that returns the free variables in a term.
fv :: Term -> [Identity]
fv (Var x)               = [x]
fv (Lambda xs e)         = (fv e) \\ xs
fv (e1 :@ e2)            = fv e1 `union` fv e2
fv _                     = []

-- | Function that checks if there are any free variables in a term.
isFv :: Identity -> Combinator -> Bool
isFv v (STGVar x)  = v == x
isFv v (e1 ::@ e2) = isFv v e1 || isFv v e2
isFv _ _           = False

isFvTyp :: Typ -> Identity -> Bool
isFvTyp (TVar a) b
    | a /= b    = False
    | otherwise = True
isFvTyp (TList a) b
    = isFvTyp a b
isFvTyp (t1 :-> t2) b
    = isFvTyp t1 b || isFvTyp t2 b
isFvTyp _ _
    = False
