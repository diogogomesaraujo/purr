module STG where
import Ast
import Fv (isFv)
import Err

data STGOperation = (::+)
               | (::-)
               | (::/)
               | (::*)
               | (::&&)
               | (::||)
               | (::==)
               | (::!=)
               | (::<)
               | (::<=)
               | (::>)
               | (::>=)
               deriving Show

data Combinator = STGVar Identity
                | Combinator ::@ Combinator -- Application
                | S | K | I | B | C
                | Y Combinator
                | STGConst Constant
                | STGPrim STGOperation
                | STGIf Combinator Combinator Combinator
                | STGLet Identity Combinator Combinator
                deriving Show

compileSTG :: Term -> Either Err Combinator

compileSTG (Const x)
    = Right $ STGConst x

compileSTG (Var v)
    = Right $ STGVar v

compileSTG (Prim (:+))
    = pure $ STGPrim (::+)

compileSTG (Prim (:-))
    = pure $ STGPrim (::-)

compileSTG (Prim (:*))
    = pure $ STGPrim (::*)

compileSTG (Prim (:/))
    = pure $ STGPrim (::/)

compileSTG (Prim (:==))
    = pure $ STGPrim (::==)

compileSTG (Prim (:!=))
    = pure $ STGPrim (::!=)

compileSTG (Prim (:&&))
    = pure $ STGPrim (::&&)

compileSTG (Prim (:||))
    = pure $ STGPrim (::||)

compileSTG (Prim (:>))
    = pure $ STGPrim (::>)

compileSTG (Prim (:>=))
    = pure $ STGPrim (::>=)

compileSTG (Prim (:<))
    = pure $ STGPrim (::<)

compileSTG (Prim (:<=))
    = pure $ STGPrim (::<=)

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

compileSTG (Let x e1 e2)
    = do e1' <- compileSTG e1
         e2' <- compileSTG e2
         pure $ STGLet x e1' e2'

compileSTG (Fix e)
    = do e' <- compileSTG e
         pure $ Y e'

compileSTG _
    = Left $ Compiling "invalid lambda term"
