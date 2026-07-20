module Compile where

import STG
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
    = do e1' <- compileSTG $ replaceVars xs e1
         e2' <- compileSTG e2
         pure $ STGLet x ::@ e1' ::@ e2'

compileSTG (LetRec x xs e1 e2)
    = do e1' <- compileSTG
                $ replaceRec x
                $ replaceVars xs e1
         e2' <- compileSTG e2
         pure $ STGLet x ::@ e1' ::@ e2'

compileSTG (Fix e)
    = do e' <- compileSTG e
         pure $ Y ::@ e'

compileSTG (e1 :@ e2)
    = do e1' <- compileSTG e1
         e2' <- compileSTG e2
         pure $ e1' ::@ e2'

compileSTG (Lambda x (Var v))
    | x == v = Right $ I

compileSTG (Lambda x e)
    | not $ isFv x e = (::@)
        <$> Right K
        <*> compileSTG e

compileSTG (Lambda x (e :@ Var x'))
    | x == x' && (not $ isFv x e) = compileSTG e

compileSTG (Lambda x (e1 :@ e2))
    | not $ isFv x e1 = do
        e1' <- compileSTG e1
        e2' <- compileSTG (Lambda x e2)
        pure $ B ::@ e1' ::@ e2'

compileSTG (Lambda x (e1 :@ e2))
    | not $ isFv x e2 = do
        e1' <- compileSTG (Lambda x e1)
        e2' <- compileSTG e2
        pure $ C ::@ e1' ::@ e2'

compileSTG (Lambda x (e1 :@ e2))
    | isFv x e1 && isFv x e2 = do
        e1' <- compileSTG (Lambda x e1)
        e2' <- compileSTG (Lambda x e2)
        pure $ S ::@ e1' ::@ e2'

compileSTG _
    = Left $ Compiling $ "invalid lambda term"

compilePrim :: Operation -> Combinator
compilePrim (:+)       = ADD
compilePrim (:-)       = SUB
compilePrim (:*)       = MUL
compilePrim (:/)       = DIV
compilePrim (:==)      = STG.EQ
compilePrim (:!=)      = DIFF
compilePrim (:&&)      = AND
compilePrim (:||)      = OR
compilePrim (:<)       = L
compilePrim (:<=)      = LE
compilePrim (:>)       = G
compilePrim (:>=)      = GE
compilePrim (Custom x) = STGVar x

-- | Function that replaces each variable in a let statement by a lambda function
-- with the variable as the argument.
replaceVars :: [Identity] -> Term -> Term
replaceVars xs e
    = foldr Lambda e xs

-- | Function that wraps the body of a let rec statement in a fixed point.
replaceRec :: Identity -> Term -> Term
replaceRec x e
    = Fix $ Lambda x e
