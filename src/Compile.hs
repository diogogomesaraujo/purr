module Compile where

import G
import Ast
import Err
import Fv

-- | Function that compiles the purr language into an internal SKI combinatory logic.
compileSTG :: Term ->  Either Err Combinator

compileSTG (Const x)
    = Right $ STGConst x

compileSTG (Var v)
    = Right $ STGVar v

compileSTG (Prim p)
    = pure $ compilePrim p

compileSTG (If e1 e2 e3)
    = do e1' <- compileSTG e1
         e2' <- compileSTG e2
         e3' <- compileSTG e3
         pure $ STGIf ::@ e1' ::@ e2' ::@  e3'

compileSTG (Let x xs e1 e2)
    = let e1' = replaceVars xs e1
    in compileSTG (Lambda x e2 :@ e1')

compileSTG (LetRec x xs e1 e2)
    = let e1' = Fix $ Lambda x $ replaceVars xs e1
    in compileSTG (Lambda x e2 :@ e1')

compileSTG (Fix e)
    = do e' <- compileSTG e
         pure $ Y ::@ e'

compileSTG (e1 :@ e2)
    = do e1' <- compileSTG e1
         e2' <- compileSTG e2
         pure $ e1' ::@ e2'

compileSTG (Lambda x e)
    = case compileSTG e of
        Left err -> Left err
        Right e' -> abstract x e'

compilePrim :: Operation -> Combinator
compilePrim (:+)       = ADD
compilePrim (:-)       = SUB
compilePrim (:*)       = MUL
compilePrim (:/)       = DIV
compilePrim (:==)      = G.EQ
compilePrim (:!=)      = DIFF
compilePrim (:&&)      = AND
compilePrim (:||)      = OR
compilePrim (:<)       = L
compilePrim (:<=)      = LE
compilePrim (:>)       = G
compilePrim (:>=)      = GE
compilePrim (:::)      = CONS
compilePrim (Custom x) = STGVar x

-- | Function that compiles a lambda function depending on the internal expression.
abstract :: Identity -> Combinator -> Either Err Combinator

abstract x (STGVar v)
    | x == v = Right $ I

abstract x e
    | not $ isFv x e = Right $ K ::@ e

abstract x (e ::@ STGVar x')
    | x == x' && (not $ isFv x e) = Right $ e

abstract x (e1 ::@ e2)
    | not (isFv x e1)
        = (::@)
        <$> ((::@)
            <$> Right B
            <*> Right e1)
        <*> abstract x e2

abstract x (e1 ::@ e2)
    | not (isFv x e2)
    = (::@)
    <$> ((::@)
        <$> Right C
        <*> abstract x e1)
    <*> Right e2

abstract x (e1 ::@ e2)
    = (::@)
    <$> ((::@)
        <$> Right S
        <*> abstract x e1)
    <*> abstract x e2

abstract _ _ = Left $ Compiling "invalid lambda term"

-- | Function that replaces each variable in a let statement by a lambda function
-- with the variable as the argument.
replaceVars :: [Identity] -> Term -> Term
replaceVars xs e
    = foldr Lambda e xs
