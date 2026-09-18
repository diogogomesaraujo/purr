module J where

import Ast
import Typed
import Data.Map as M
import Data.List as L
import Data.Set as S
import Control.Monad (foldM)

type Ctr = Int

type J = (Ctr, Aliases)

-- type inference

data UnifyErr = RecursiveType Identity Mono
                | Impossible Mono Mono
                | UnknownVar Identity

instance Show (UnifyErr) where
    show (RecursiveType i t) = i
                                ++ " is has recursive type "
                                ++ Prelude.show t
    show (Impossible t1 t2)  = Prelude.show t1
                                ++ " and " ++ Prelude.show t2
                                ++ " are impossible to unify"
    show (UnknownVar i)      = i ++ " is an unknown variable"

type InferResult a = Either UnifyErr a

infer :: J -> Term -> InferResult (Poly, J)
infer j e
    = do (t, (ctr, aliases)) <- infer' j e stdEnv
         pure $ ( gen stdEnv $ canonicalize aliases t
                , (ctr, aliases))

infer' :: J -> Term -> Env -> InferResult (Mono, J)
infer' j (Const c) env
    = inferConst j c env
infer' j (Var v) env
    = inferVar j v env
infer' j (e1 :@ e2) env
    = inferApp j e1 e2 env
infer' j (Lambda xs e) env
    = inferLambda j xs e env
infer' j (TypedLambda xs dt e) env
    = inferTypedLambda j xs dt e env
infer' j (Let x xs e1 e2) env
    = inferLet j x xs e1 e2 env
infer' j (TypedLet x xs dt e1 e2) env
    = inferTypedLet j x xs dt e1 e2 env
infer' j (LetRec x xs e1 e2) env
    = inferLetRec j x xs e1 e2 env
infer' j (TypedLetRec x xs dt e1 e2) env
    = inferTypedLetRec j x xs dt e1 e2 env
infer' j (If e1 e2 e3) env
    = inferIf j e1 e2 e3 env
infer' j (Fix e) env
    = inferFix j e env
infer' j (Prim p) env
    = inferPrim j p env

inferConst :: J -> Constant -> Env -> InferResult (Mono, J)
inferConst j (CBool _) _
    = pure (TConst "bool", j)
inferConst j (CInt _) _
    = pure (TConst "int", j)
inferConst j (CFloat _) _
    = pure (TConst "float", j)
inferConst j (CList l) env
    = do (v, j') <- pure $ newVar j
         j''    <- foldM (\acc e -> do (t, je) <- infer' acc e env
                                       unify je (t, TVar v)) j' l
         pure (TApp "list" [TVar v], j'')

inferVar :: J -> Identity -> Env -> InferResult (Mono, J)
inferVar j v env
    = case L.lookup v env of
           Just (alphas, t) ->
                let (new_vars, j') = newVars j
                               $ L.length alphas in
                pure $ (inst (S.fromList new_vars) (alphas, t), j')
           Nothing -> Left $ UnknownVar v

inferApp :: J -> Term -> Term -> Env -> InferResult (Mono, J)
inferApp j e1 e2 env
    = do (t1, j1) <- infer' j e1 env
         (t2, j2) <- infer' j1 e2 env
         (t', j') <- pure $ newVar j2
         j''      <- unify j' (t1, arrow t2 (TVar t'))
         pure $ (TVar t', j'')

inferLambda :: J -> [Identity] -> Term -> Env -> InferResult (Mono, J)
inferLambda j [] e env
    = infer' j e env
inferLambda j xs e env
    = let (ts, j') = newVars j $ L.length xs
          env'      = L.foldl (\acc (x, t) ->
                                bind acc x $ mono (TVar t))
                        env (zip xs ts) in
      do (t', j'') <- infer' j' e env'
         tvs       <- pure $ L.foldr (\t acc -> arrow (TVar t) acc)
                           t'
                           ts
         pure $ (tvs, j'')

inferTypedLambda :: J -> [Identity] -> DeclaredType -> Term -> Env -> InferResult (Mono, J)
inferTypedLambda j xs dt e env
    = do dt'       <- pure $ fromDeclaredType dt
         (t, j')   <- inferLambda j xs e env
         j''       <- unify j' (dt', t)
         pure (t, j'')

inferLet :: J -> Identity -> [Identity] -> Term -> Term -> Env -> InferResult (Mono, J)
inferLet (ctr, aliases) x xs e1 e2 env
    = let e1' = Lambda xs e1 in
      do (t, j) <- infer' (ctr, aliases) e1' env
         t'     <- pure
                   $ gen env
                   $ canonicalize (snd j) t
         env'   <- pure $ bind env x t'
         infer' j e2 env'

inferTypedLet :: J -> Identity -> [Identity] -> DeclaredType -> Term -> Term -> Env -> InferResult (Mono, J)
inferTypedLet (ctr, aliases) x xs dt e1 e2 env
    = let e1' = Lambda xs e1 in
      do (t, j) <- infer' (ctr, aliases) e1' env
         dt'    <- pure $ fromDeclaredType dt
         j'     <- unify j (t, dt')
         t'     <- pure
                   $ gen env
                   $ canonicalize (snd j) t
         env'   <- pure $ bind env x t'
         infer' j' e2 env'

inferLetRec :: J -> Identity -> [Identity] -> Term -> Term -> Env -> InferResult (Mono, J)
inferLetRec j x xs e1 e2 env
    = let e1' = Fix $ Lambda (x:xs) e1 in
      inferLet j x [] e1' e2 env

inferTypedLetRec :: J -> Identity -> [Identity] -> DeclaredType -> Term -> Term -> Env -> InferResult (Mono, J)
inferTypedLetRec j x xs dt e1 e2 env
    = let e1' = Fix $ Lambda (x:xs) e1 in
      inferTypedLet j x [] dt e1' e2 env

inferIf :: J -> Term -> Term -> Term -> Env -> InferResult (Mono, J)
inferIf j e1 e2 e3 env
    = infer' j (Var "if" :@ e1 :@ e2 :@ e3) env

inferFix :: J -> Term -> Env -> InferResult (Mono, J)
inferFix j e env
    = infer' j (Var "fix" :@ e) env

inferPrim :: J -> Operation -> Env -> InferResult (Mono, J)
inferPrim j p env
    = infer' j (Var $ showOp p) env

-- algorithm J

newJ :: J
newJ = (0, M.empty)

-- unify

unify :: J -> (Mono, Mono) -> InferResult J
unify (ctr, aliases) (t1, t2)
    = let ct1 = canonicalize aliases t1
          ct2 = canonicalize aliases t2 in
      case (ct1, ct2) of
           (ct1', ct2') | ct1' == ct2' -> Right (ctr, aliases)
           (TApp f1 ts1, TApp f2 ts2)
                | f1 == f2
                  && length ts1 == length ts2 ->
                      let iter = L.zip ts1 ts2 in
                      foldM (\j (t1', t2') ->
                          unify j (t1', t2')) (ctr, aliases) iter
           (TVar v, t) | occurs t v -> Left $ RecursiveType v t
                       | otherwise  -> Right (ctr, M.insert v t aliases)
           (t, TVar v) -> unify (ctr, aliases) (TVar v, t)
           (tt1, tt2) -> Left $ Impossible tt1 tt2

-- new variables

newVar :: J -> (Identity, J)
newVar (ctr, aliases)
    = ("_" ++ Prelude.show ctr,
        (ctr + 1, aliases))

newVars :: J -> Int -> ([Identity], J)
newVars j count
    = let (l, j') = L.foldl (\(acc, j'') _ ->
            let (new_v, new_j) = newVar j'' in
            (acc ++ [new_v], new_j)) ([], j)
            $ L.replicate count () in
      (l, j')
