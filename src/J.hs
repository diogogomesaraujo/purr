module J where

import Ast
import Typed
import Data.Map as M
import Data.List as L
import Data.Set as S

type Ctr = Int

type J = (Ctr, Aliases)

data UnifyErr = RecursiveType | Impossible

type InferResult a = Either UnifyErr a

infer' :: J -> Term -> Env -> InferResult (Mono, J)
infer' j (Var v) env
    = case L.lookup v env of
           Just (alphas, t) ->
                let (new_vars, j') = newVars j
                               $ L.length alphas in
                return $ (inst new_vars (alphas, t), j')
infer' j (e1 :@ e2) env
    = do (t1, j1) <- infer' j e1 env
         (t2, j2) <- infer' j1 e2 env
         (t', j') <- pure $ newVar j2
         j''      <- unify j' (t1, arrow t2 (TVar t'))
         pure $ (TVar t', j'')
infer' j (Lambda xs e) env
    = let (ts, j') = newVars j $ L.length xs
          env'      = L.foldl (\acc (x, t) ->
                                bind acc x $ mono (TVar t))
                        env (zip xs (S.toList ts)) in
      do (t', j'') <- infer' j' e env'
         tvs      <-
            let ts' = S.toList ts in
            pure $ L.foldl (\acc t -> arrow acc (TVar t))
                           (TVar $ L.head ts')
                           (L.tail ts')
         pure $ (arrow tvs t', j'')


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
                      L.foldl (\acc (t1', t2') ->
                          case acc of
                            Left e -> Left e
                            _      -> unify (ctr, aliases)
                                            (t1', t2')) (Right (ctr, aliases))
                                            iter
           (TVar v, t) | occurs t v -> Left RecursiveType
                       | otherwise  -> Right (ctr, M.insert v t aliases)
           _ -> Left Impossible



-- new variables

newVar :: J -> (Identity, J)
newVar (ctr, aliases)
    = ("_" ++ show ctr,
        (ctr + 1, aliases))

newVars :: J -> Int -> (Set Identity, J)
newVars j count
    = let (l, j') = L.foldl (\(acc, j') _ ->
            let (new_v, new_j) = newVar j' in
            (acc ++ [new_v], new_j)) ([], j)
            $ L.replicate count () in
      (S.fromList l, j')
