module Eval where

import Ast
import Err
import qualified J
import qualified Typed
import Text.Printf (printf)

data Value = VInt Int
           | VFloat Float
           | VBool Bool
           | VList [Value]
           | VClosure [Identity] Term

type EvalResult = Either Err Term

type ValueResult = Either Err Value

eval :: Term -> ValueResult
eval e = case eval' e of
              Right e' -> eval e'
              Left _   -> toValue e

eval' :: Term -> EvalResult
eval' (Lambda [] e)
    = return e
eval' (Lambda [x] e1 :@ e2)
    = return $ subst x e2 e1
eval' (Lambda (x:xs) e1 :@ e2)
    = return
      $ Lambda xs
      $ subst x e2 e1
eval' (TypedLambda xs _ e1 :@ e2)
    = eval' $ Lambda xs e1 :@ e2
eval' (If (Const (CBool True)) e2 _)
    = return e2
eval' (If (Const (CBool False)) _ e3)
    = return e3
eval' (If e1 e2 e3)
    = do e1' <- eval' e1
         return $ If e1' e2 e3
eval' (Let x xs e1 e2)
    = return $ subst x (lambda xs e1) e2
eval' (TypedLet x xs _ e1 e2)
    = eval' $ Let x xs e1 e2
eval' (LetRec x xs e1 e2)
    = return $ subst x (Fix $ lambda (x:xs) e1) e2
eval' (TypedLetRec x xs _ e1 e2)
    = eval' $ LetRec x xs e1 e2
eval' (Fix e)
    = return $ e :@ Fix e
eval' (Prim (:&&) :@ (Const (CBool False)) :@ _)
    = return $ Const $ CBool False
eval' (Prim (:&&) :@ (Const (CBool True)) :@ e)
    = return e
eval' (Prim (:&&) :@ e1 :@ e2)
    = do e1' <- eval' e1
         return $ Prim (:&&) :@ e1' :@ e2
eval' (Prim (:||) :@ (Const (CBool True)) :@ _)
    = return $ Const $ CBool True
eval' (Prim (:||) :@ (Const (CBool False)) :@ e)
    = return e
eval' (Prim (:||) :@ e1 :@ e2)
    = do e1' <- eval' e1
         return $ Prim (:||) :@ e1' :@ e2
eval' (Prim (:::) :@ e :@ Const (CList l))
    = return $ Const $ CList $ e:l
eval' (Prim (:::) :@ e1 :@ e2)
    = do e2' <- eval' e2
         return $ (Prim (:::) :@ e1 :@ e2')
eval' (Prim p :@ Const c1 :@ Const c2)
    = evalPrim p c1 c2
eval' (Prim p :@ (Const c) :@ e2)
    = do e2' <- eval' e2
         return $ Prim p :@ Const c :@ e2'
eval' (Prim p :@ e1 :@ e2)
    = do e1' <- eval' e1
         return $ Prim p :@ e1' :@ e2
eval' (Var "head" :@ Const (CList (x:_)))
    = return x
eval' (Var "head" :@ Const (CList []))
    = Left $ Evaluating "the list is empty"
eval' (Var "head" :@ e)
    = do e' <- eval' e
         return $ Var "head" :@ e'
eval' (Var "tail" :@ Const (CList (_:xs)))
    = return $ Const $ CList xs
eval' (Var "tail" :@ Const (CList _))
    = Left $ Evaluating "the list has no tail"
eval' (Var "tail" :@ e)
    = do e' <- eval' e
         return $ Var "tail" :@ e'
eval' (e1 :@ e2)
    = do e1' <- eval' e1
         return $ e1' :@ e2
eval' _ = Left $ Evaluating "couldn't reduce the term further"

evalPrim :: Operation -> Constant -> Constant -> EvalResult
evalPrim (:+) (CInt i1) (CInt i2)
    = return $ Const $ CInt $ i1 + i2
evalPrim (:+) (CFloat f1) (CFloat f2)
    = return $ Const $ CFloat $ f1 + f2
evalPrim (:-) (CInt i1) (CInt i2)
    = return $ Const $ CInt $ i1 - i2
evalPrim (:-) (CFloat f1) (CFloat f2)
    = return $ Const $ CFloat $ f1 - f2
evalPrim (:*) (CInt i1) (CInt i2)
    = return $ Const $ CInt $ i1 - i2
evalPrim (:*) (CFloat f1) (CFloat f2)
    = return $ Const $ CFloat $ f1 - f2
evalPrim (:/) (CInt _) (CInt 0)
    = Left $ Evaluating "division by zero"
evalPrim (:/) (CFloat _) (CFloat 0.0)
    = Left $ Evaluating "division by zero"
evalPrim (:/) (CInt i1) (CInt i2)
    = return $ Const $ CInt $ i1 `div` i2
evalPrim (:/) (CFloat f1) (CFloat f2)
    = return $ Const $ CFloat $ f1 / f2
evalPrim (:>) (CInt i1) (CInt i2)
    = return $ Const $ CBool $ i1 > i2
evalPrim (:>) (CFloat f1) (CFloat f2)
    = return $ Const $ CBool $ f1 > f2
evalPrim (:<) (CInt i1) (CInt i2)
    = return $ Const $ CBool $ i1 < i2
evalPrim (:<) (CFloat f1) (CFloat f2)
    = return $ Const $ CBool $ f1 < f2
evalPrim (:>=) (CInt i1) (CInt i2)
    = return $ Const $ CBool $ i1 >= i2
evalPrim (:>=) (CFloat f1) (CFloat f2)
    = return $ Const $ CBool $ f1 >= f2
evalPrim (:<=) (CInt i1) (CInt i2)
    = return $ Const $ CBool $ i1 <= i2
evalPrim (:<=) (CFloat f1) (CFloat f2)
    = return $ Const $ CBool $ f1 <= f2
evalPrim (:==) (CInt i1) (CInt i2)
    = return $ Const $ CBool $ i1 == i2
evalPrim (:==) (CFloat f1) (CFloat f2)
    = return $ Const $ CBool $ f1 == f2
evalPrim (:==) (CBool b1) (CBool b2)
    = return $ Const $ CBool $ b1 == b2
evalPrim (:==) (CList (x:xs)) (CList (y:ys))
    = case (eval' x, eval' y) of
           (Right (Const cx)
            , Right (Const cy)) ->
                do xy  <- evalPrim (:==) cx cy
                   xys <- evalPrim (:==) (CList xs) (CList ys)
                   return $ Prim (:&&) :@ xy :@ xys
           _ -> Left $ Evaluating "can't compare non-constant values"
evalPrim (:==) (CList []) (CList [])
    = return $ Const $ CBool $ True
evalPrim (:==) (CList []) (CList _)
    = return $ Const $ CBool $ False
evalPrim (:==) (CList _) (CList [])
    = return $ Const $ CBool $ False
evalPrim (:!=) (CInt i1) (CInt i2)
    = return $ Const $ CBool $ i1 /= i2
evalPrim (:!=) (CFloat f1) (CFloat f2)
    = return $ Const $ CBool $ f1 /= f2
evalPrim (:!=) (CBool b1) (CBool b2)
    = return $ Const $ CBool $ b1 /= b2
evalPrim (:!=) (CList (x:xs)) (CList (y:ys))
    = case (eval' x, eval' y) of
           (Right (Const cx)
            , Right (Const cy)) ->
                do xy  <- evalPrim (:!=) cx cy
                   xys <- evalPrim (:!=) (CList xs) (CList ys)
                   return $ Prim (:&&) :@ xy :@ xys
           _ -> Left $ Evaluating "can't compare non-constant values"
evalPrim (:!=) (CList []) (CList [])
    = return $ Const $ CBool $ False
evalPrim (:!=) (CList []) (CList _)
    = return $ Const $ CBool $ True
evalPrim (:!=) (CList _) (CList [])
    = return $ Const $ CBool $ True
evalPrim p _ _ = Left $ Evaluating $ "invalid" ++ showOp p ++ " usage"

subst :: Identity -> Term -> Term -> Term
subst x es (Var v)
    | v == x    = es
    | otherwise = Var v
subst x es (Lambda xs e)
    | x `elem` xs = Lambda xs e
    | otherwise   = Lambda xs $ subst x es e
subst x es (TypedLambda xs _ e)
    = subst x es (Lambda xs e)
subst x es (e1 :@ e2)
    = subst x es e1 :@ subst x es e2
subst x es (If e1 e2 e3)
    = If (subst x es e1)
         (subst x es e2)
         (subst x es e3)
subst x es (Let v xs e1 e2)
    | x == v
        = Let v xs e1 e2
    | x `elem` xs
        = Let v xs e1 (subst x es e2)
    | otherwise
        = Let v xs (subst x es e1)
                   (subst x es e2)
subst x es (TypedLet v xs _ e1 e2)
    = subst x es (Let v xs e1 e2)
subst x es (LetRec v xs e1 e2)
    | x == v
        = LetRec v xs e1 e2
    | x `elem` xs
        = LetRec v xs e1 (subst x es e2)
    | otherwise
        = LetRec v xs (subst x es e1)
               (subst x es e2)
subst x es (TypedLetRec v xs _ e1 e2)
    = subst x es (LetRec v xs e1 e2)
subst x es (Fix e)
    = Fix $ subst x es e
subst x es (Prim o)
    | x == showOp o = es
    | otherwise     = Prim o
subst _ _ e = e

toValue :: Term -> ValueResult
toValue (Const (CInt i))
    = return $ VInt i
toValue (Const (CFloat f))
    = return $ VFloat f
toValue (Const (CBool f))
    = return $ VBool f
toValue (Const (CList xs))
    = do xs' <- mapM (\x -> case toValue x of
                                 Right x' -> return x'
                                 Left _   -> eval x) xs
         return $ VList xs'
toValue (Lambda xs e)
    = return $ VClosure xs e
toValue _
    = Left $ Evaluating "expression is not a value"

showValue :: Value -> String
showValue (VInt i)       = printf "%d" i
showValue (VFloat f)     = printf "%f" f
showValue (VBool True)   = "true"
showValue (VBool False)  = "false"
showValue (VList xs)
    = let l = foldl (\acc x -> acc ++ "," ++ showValue x) "" xs in
      case l of
      []    -> "[]"
      _:l' -> "[" ++ l' ++ "]"
showValue (VClosure xs e)
    = case J.infer J.newJ (Lambda xs e) of
           Right (t, _) -> "fn: " ++ Typed.showPoly t
           Left _       -> "couldn't infer type of closure"

lambda :: [Identity] -> Term -> Term
lambda [] e = e
lambda xs e = Lambda xs e
