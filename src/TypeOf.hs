module TypeOf where

import Ast
import Err
import Typed
import Unify

-- | Function that returns the type of a term or fails doing so, if it does not respect the language's type system.
typeOf :: StaticEnv -> Term -> EitherTypSubst

typeOf _ (Const (CBool _))
    = Right (TBool, [])

typeOf _ (Const (CInt _))
    = Right (TInt, [])

typeOf _ (Const (CFloat _))
    = Right (TFloat, [])

typeOf env (Const (CList l))
    = typeOfList l env

typeOf env (Var v)
    = maybe (Left $ Compiling $ "unbound variable: " ++ v) (\x -> Right (x, [])) (lookup v env)

typeOf env (TypedLet x xs t e1 e2)
    = typeOfLet x xs t e1 e2 env

typeOf env (TypedLetRec x xs t e1 e2)
    = typeOfLetRec x xs t e1 e2 env

typeOf env (TypedLambda xs t e)
    = typeOfLambda xs t e env

typeOf env (If e1 e2 e3)
    = typeOfIf e1 e2 e3 env

typeOf env (Fix e)
    = typeOfFix e env

typeOf env (Prim (:+) :@ e1)
    = typeOfArith "+" e1 env

typeOf env (Prim (:-) :@ e1)
    = typeOfArith "-" e1 env

typeOf env (Prim (:*) :@ e1)
    = typeOfArith "*" e1 env

typeOf env (Prim (:/) :@ e1)
    = typeOfArith "/" e1 env

typeOf env (Prim (:==) :@ e1)
    = typeOfComp e1 env

typeOf env (Prim (:!=) :@ e1)
    = typeOfComp e1 env

typeOf env (Prim (:<) :@ e1)
    = typeOfComp e1 env

typeOf env (Prim (:<=) :@ e1)
    = typeOfComp e1 env

typeOf env (Prim (:>) :@ e1)
    = typeOfComp e1 env

typeOf env (Prim (:>=) :@ e1)
    = typeOfComp e1 env

typeOf env (Prim (:&&) :@ e1)
    = typeOfProp "&&" e1 env

typeOf env (Prim (:||) :@ e1)
    = typeOfProp "||" e1 env

typeOf env (Prim (:::) :@ e1)
    = do typeOfCons e1 env

typeOf env (Prim (Custom p))
    = do typeOfCustom p env

typeOf env (e1 :@ e2)
    = typeOfApp e1 e2 env

typeOf _ e = Left $ Compiling $ "todo: the term was " ++ show e

-- | Function that checks the type of a list.
typeOfList :: [Term] -> StaticEnv -> EitherTypSubst
typeOfList [] env
    = Right (TList $ TVar $ newTVarName env, [])
typeOfList l env
    = typeOfList' l $ TVar $ newTVarName env
        where typeOfList' [] t = Right $ (TList t, [])
              typeOfList' (x:xs) t
                    = do (t', s) <- typeOf env x
                         case unify t t' s of
                            Right s' -> typeOfList' xs (apply s' t)
                            _       -> Left $ Compiling $ "types in list don't match" --todo

-- | Function that checks the type of an if term.
typeOfIf :: Term -> Term -> Term -> StaticEnv -> EitherTypSubst
typeOfIf e1 e2 e3 env
    = do (t1', s1) <- typeOf env e1
         case unify t1' TBool s1 of
            Right s -> do (t2, s2) <- typeOf env e2
                          (t3, s3) <- typeOf env e3
                          case unify t2 t3 (s ++ s2 ++ s3) of
                               Right s' -> Right (apply s t2, s')
                               _       -> Left
                                          $ Compiling
                                          $ "the type "
                                            ++ show t2
                                            ++ " does not match the type of the other expression"
            _ -> Left
                $ Compiling
                $ "the type of "
                    ++ show e1
                    ++ " does not match the type of the other expression"

-- | Function that checks the type of an fixpoint term.
typeOfFix :: Term -> StaticEnv -> EitherTypSubst
typeOfFix e env
    = do (t, s) <- typeOf env e
         s' <- unify t ((TVar "a" :-> TVar "a") :-> TVar "a") s
         Right (apply s' t, s')

-- | Function that checks the type of an application.
typeOfApp :: Term -> Term -> StaticEnv -> EitherTypSubst
typeOfApp e1 e2 env
    = case typeOf env e1 of
       Right (t1 :-> t2, s) -> do
            (t2', s2) <- typeOf env e2
            case unify t1 t2' (s ++ s2) of
                Right s1 -> Right $ (apply s1 t2, s1)
                _        -> Left
                            $ Compiling
                            $ "the type of  "
                                ++ show e1 ++ ": "    ++ show t1
                                ++ " does not match " ++ show e2
                                ++ ": "    ++ show t2'
       Right (TVar t, s) -> do (_, s2) <- typeOf env e2
                               s'       <- pure $ s ++ s2
                               t'       <- pure $ TVar $ newTVarName env
                               case unify (TVar t) t' s' of
                                    Right s'' -> Right (apply s'' t', s'')
                                    Left e    -> Left e
       Right t           -> Left
                            $ Compiling
                            $ "app: "
                                ++ show t
                                ++ "is an invalid type for the first argument of an application"
       Left e            -> Left e

-- | Function that checks the type of an let term.
typeOfLet :: Identity -> [Identity] -> DeclaredType -> Term -> Term -> StaticEnv -> EitherTypSubst
typeOfLet x xs t e1 e2 env
    = do  t'       <- pure $ typFromDeclaredType t
          env'     <- pure $ foldArgs xs env t'
          (t1, s1)       <- typeOf env' e1
          case unify t1 (absTyp t') s1 of
            Right s -> typeOf (applyToEnv ((x, t'):env') s) e2
            _       -> typesDifferErr e1 t1 e1 (absTyp t') "let"

-- | Function that checks the type of a let rec term.
typeOfLetRec :: Identity -> [Identity] -> DeclaredType -> Term -> Term -> StaticEnv -> EitherTypSubst
typeOfLetRec x xs t e1 e2 env
    = do  t'       <- pure $ typFromDeclaredType t
          env'     <- pure $ (x, t'):foldArgs xs env t'
          (t1, s)       <- typeOf env' e1
          case unify t1 (absTyp t') s of
                Right s' -> typeOf (applyToEnv env' s') e2
                _       -> typesDifferErr e1 t1 e1 (absTyp t') "let rec"

-- | Function that checks the type of a lambda term.
typeOfLambda :: [Identity] -> DeclaredType -> Term -> StaticEnv -> EitherTypSubst
typeOfLambda xs t e env
    = let t1       = typFromDeclaredType t
          env'     = foldArgs xs env t1 in
      do (t2, s) <- typeOf env' e
         case unify t2 (absTyp t1) [] of
            Right s' -> Right (apply s t1, s')
            _        -> typesDifferErr e t2 e t1 "lambda"

-- | Function that checks the type of a comparison.
typeOfComp :: Term -> StaticEnv -> EitherTypSubst
typeOfComp e env
    = do (t, s) <- typeOf env e
         Right (t :-> TBool, s)

-- | Function that checks the type of a propositional logic conditional.
typeOfProp :: String -> Term -> StaticEnv -> EitherTypSubst
typeOfProp p e env
    = do (t, s) <- typeOf env e
         case unify t TBool s of
            Right s' -> Right (TBool :-> TBool, s')
            _       -> Left
                       $ Compiling
                       $ p
                        ++ ": "
                        ++ "arguments must be bool, not "
                        ++ show t

-- | Function that checks the type of an arithmetic operation.
typeOfArith :: Identity -> Term -> StaticEnv -> EitherTypSubst
typeOfArith p e env =
    do (t, s) <- typeOf env e
       case unify t TInt s of
            Right si -> Right (TInt :-> TInt, si)
            _       ->
                case unify t TFloat s of
                    Right sf -> Right (TFloat :-> TFloat, sf)
                    _       -> Left
                               $ Compiling
                               $ p
                                    ++ ": "
                                    ++ "arguments must be either float or int, not "
                                    ++ show t

-- | Function that checks the type of a cons operation.
typeOfCons :: Term -> StaticEnv -> EitherTypSubst
typeOfCons e env
    = do (t, s) <- typeOf env e
         Right $ (apply s (TList t :-> TList t), s)

-- | Function that checks the type of a cons operation.
typeOfCustom :: Identity -> StaticEnv -> EitherTypSubst
typeOfCustom p env
    = case lookup p env of
           Just t  -> Right (t, [])
           Nothing -> Left $ Compiling $ p ++ ": not declared"

-- | Function that returns the difference between the types of two expressions that should return the same type.
typesDifferErr :: Term -> Typ -> Term -> Typ -> String -> Either Err a
typesDifferErr e1 t1 e2 t2 cont
    = Left
      $ Compiling
      $ cont ++ ": the expression "
          ++ show e1 ++ " with type "
          ++ show t1 ++ " does not match "
          ++ show e2 ++ " with type "
          ++ show t2

-- | Function that adds the arguments' types to the static environment.
foldArgs :: [Identity] -> StaticEnv -> Typ -> StaticEnv
foldArgs (x:xs) env (t1 :-> t2)
    = (x, t1):foldArgs xs env t2
foldArgs _ env _ = env
