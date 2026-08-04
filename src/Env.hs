module Env where

import Ast
import Err
import Typed

type StaticEnv = [(Identity, Typ)]

typeOf :: StaticEnv -> Term -> Either Err Typ

typeOf _ (Const (CBool _))
    = Right TBool

typeOf _ (Const (CInt _))
    = Right TInt

typeOf _ (Const (CFloat _))
    = Right TFloat

typeOf env (Var v)
    = maybe (Left $ Compiling $ "unbound variable: " ++ v) pure (lookup v env)

typeOf env (TypedLet x xs t e1 e2)
    = typeOfLet x xs t e1 e2 env

typeOf env (TypedLetRec x xs t e1 e2)
    = typeOfLetRec x xs t e1 e2 env

typeOf env (TypedLambda xs t e)
    = typeOfLambda xs t e env

typeOf env (If e1 e2 e3)
    = if typeOf env e1 == Right TBool
        then let t2 = typeOf env e2
        in if t2 == typeOf env e3
            then t2
            else Left
                $ Compiling
                $ "the type "
                    ++ show t2
                    ++ " does not match the type of the other expression"
        else Left
            $ Compiling
            $ "the type of "
                ++ show e1
                ++ " does not match the type of the other expression"

typeOf env (Fix e1)
    = case typeOf env e1 of
           Right (_ :-> e1') -> Right e1'
           _                 -> Left $ Compiling "fixpoint without lambda function argument"

typeOf env (e1 :@ e2)
    = case typeOf env e1 of
           Right (t1 :-> t2) ->
                let t2' = typeOf env e2
                in if Right t1 == t2'
                    then Right t2
                    else Left
                        $ Compiling
                        $ "the type of  "
                            ++ show e1 ++ ": "    ++ show t1
                            ++ " does not match " ++ show e2
                            ++ ": "    ++ show t2'
           _               -> Left
                            $ Compiling
                            $ "couldn't type check " ++ show e1 -- todo fix messages

typeOf _ (Prim (:+)) = Right $ TInt :-> TInt :-> TInt -- temporary

typeOf _ _ = Left $ Compiling "todo"

typeOfLet :: Identity -> [Identity] -> DeclaredType -> Term -> Term -> StaticEnv -> Either Err Typ
typeOfLet x xs t e1 e2 env
    = let t'       = typFromDeclaredType t
          env'     = foldArgs xs env t'
      in if typeOf env' e1 == Right (absTyp t')
            then typeOf ((x, t'):env') e2
            else Left $ Compiling $ "the type of " ++ show e1 ++ ": " ++ (show $ typeOf env' e1) ++ " does not match " ++ (show $ absTyp t')

typeOfLetRec :: Identity -> [Identity] -> DeclaredType -> Term -> Term -> StaticEnv -> Either Err Typ
typeOfLetRec x xs t e1 e2 env
    = let t'       = typFromDeclaredType t
          env'     = (x, t' :-> absTyp t'):foldArgs xs env t'
      in if typeOf env' e1 == Right (absTyp t')
            then typeOf env' e2
            else Left $ Compiling $ "the type of " ++ show e1 ++ " does not match " ++ (show $ absTyp t' )

typeOfLambda :: [Identity] -> DeclaredType -> Term -> StaticEnv -> Either Err Typ
typeOfLambda xs t e env
    = let t'       = typFromDeclaredType t
          env'     = foldArgs xs env t'
      in typeOf env' e

foldArgs :: [Identity] -> StaticEnv -> Typ -> StaticEnv
foldArgs (x:xs) env (t1 :-> t2)
    = (x, t1):foldArgs xs env t2
foldArgs _ env _
    = env
