module Compile where

import STG
import Ast
import Err
import Fv

-- | Function that compiles the purr language into an internal SKI combinatory logic.
compileSTG :: Term -> Either Err Combinator

compileSTG (Const x)
    = Right $ STGConst x

compileSTG (Var v)
    = Right $ STGVar v

compileSTG (Prim p)
    = pure $ STGPrim p

compileSTG (App e1 e2)
    = do e1' <- compileSTG e1
         e2' <- compileSTG e2
         pure $ e1' ::@ e2'

compileSTG (Lambda x (Var v))
    | x == v = Right $ I

compileSTG (Lambda x e)
    | not $ isFv x e = (::@)
        <$> Right K
        <*> compileSTG e

compileSTG (Lambda x (App e (Var x')))
    | x == x' && (not $ isFv x e) = compileSTG e

compileSTG (Lambda x (App e1 e2))
    | not $ isFv x e1 = do
        e1' <- compileSTG e1
        e2' <- compileSTG (Lambda x e2)
        pure $ B ::@ e1' ::@ e2'

compileSTG (Lambda x (App e1 e2))
    | not $ isFv x e2 = do
        e1' <- compileSTG (Lambda x e1)
        e2' <- compileSTG e2
        pure $ C ::@ e1' ::@ e2'

compileSTG (Lambda x (App e1 e2))
    | isFv x e1 && isFv x e2 = do
        e1' <- compileSTG (Lambda x e1)
        e2' <- compileSTG (Lambda x e2)
        pure $ S ::@ e1' ::@ e2'

compileSTG (If e1 e2 e3)
    = do e1' <- compileSTG e1
         e2' <- compileSTG e2
         e3' <- compileSTG e3
         pure $ STGIf e1' e2' e3'

compileSTG (Let x xs e1 e2)
    = do e1' <- compileSTG $ replaceVars xs e1
         e2' <- compileSTG e2
         pure $ STGLet x e1' e2'

compileSTG (LetRec x xs e1 e2)
    = do e1' <- compileSTG
                $ replaceRec x
                $ replaceVars xs e1
         e2' <- compileSTG e2
         pure $ STGLet x e1' e2'

compileSTG (LetOp x x1 x2 e1 e2)
    = do e1' <- compileSTG $ replaceVars [x1, x2] e1
         e2' <- compileSTG e2
         pure $ STGLet x e1' e2'

compileSTG (Fix e)
    = do e' <- compileSTG e
         pure $ Y e'

compileSTG (Lst l)
    = do l' <- compileListSTG l
         pure $ STGList l'

compileSTG _
    = Left $ Compiling "invalid lambda term"

compileListSTG :: [Term] -> Either Err [Combinator]
compileListSTG []     = pure []
compileListSTG (x:xs)
    = case compileSTG x of
        Right c -> (:) <$> pure c <*> compileListSTG xs
        Left e  -> Left e

-- | Function that replaces each variable in a let statement by a lambda function
-- with the variable as the argument.
replaceVars :: [Identity] -> Term -> Term
replaceVars xs e
    = foldl (\acc x -> Lambda x acc) e xs

-- | Function that wraps the body of a let rec statement in a fixed point.
replaceRec :: Identity -> Term -> Term
replaceRec x e
    = Fix $ Lambda x e
