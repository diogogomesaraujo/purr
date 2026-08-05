module TypeOf where

import Ast
import Err
import Typed
import Unify
import Debug

typeOf :: StaticEnv -> Term -> EitherTyp

typeOf _ (Const (CBool _))
    = Right TBool

typeOf _ (Const (CInt _))
    = Right TInt

typeOf _ (Const (CFloat _))
    = Right TFloat

typeOf env (Const (CList l))
    = typeOfList l env

typeOf env (Var v)
    = maybe (Left $ Compiling $ "unbound variable: " ++ v) pure (lookup v env)

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
    = typeOfComp "==" e1 env

typeOf env (Prim (:!=) :@ e1)
    = typeOfComp "!=" e1 env

typeOf env (Prim (:<) :@ e1)
    = typeOfComp "<" e1 env

typeOf env (Prim (:<=) :@ e1)
    = typeOfComp "<=" e1 env

typeOf env (Prim (:>) :@ e1)
    = typeOfComp ">" e1 env

typeOf env (Prim (:>=) :@ e1)
    = typeOfComp ">=" e1 env

typeOf env (Prim (:&&) :@ e1)
    = typeOfProp "&&" e1 env

typeOf env (Prim (:||) :@ e1)
    = typeOfProp "||" e1 env

typeOf env (Prim (:::) :@ e1 :@ e2)
    = typeOfCons e1 e2 env

typeOf env (e1 :@ e2)
    = typeOfApp e1 e2 env

typeOf _ _ = Left $ Compiling "todo"

typeOfList :: [Term] -> StaticEnv -> EitherTyp
typeOfList [] env
    = Right $ TList $ TVar $ newTVarName env
typeOfList l env
    = typeOfList' l $ TVar $ newTVarName env
        where typeOfList' [] t = Right $ TList t
              typeOfList' (x:xs) t
                    = do t' <- typeOf env x
                         case unify t t' [] of
                            Right s -> typeOfList' xs (apply s t)
                            _       -> Left $ Compiling $ "types in list don't match" --todo

typeOfIf :: Term -> Term -> Term -> StaticEnv -> EitherTyp
typeOfIf e1 e2 e3 env
    = do t1' <- typeOf env e1
         case unify t1' TBool [] of
            Right _ -> do t2 <- typeOf env e2
                          t3 <- typeOf env e3
                          case unify t2 t3 [] of
                            Right s -> Right $ apply s t2
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

typeOfFix :: Term -> StaticEnv -> EitherTyp
typeOfFix e env
    = case typeOf env e of
           Right (_ :-> e1') -> Right e1'
           _                 -> Left
                                $ Compiling
                                $ "tried to apply non-function term to a fixpoint"
                                    ++ show e ++ "with type "
                                    ++ (show $ typeOf env e)

typeOfApp :: Term -> Term -> StaticEnv -> EitherTyp
typeOfApp e1 e2 env
    = case typeOf env e1 of
       Right (t1 :-> t2) -> do
            t2' <- typeOf env e2
            case unify t1 t2' [] of
                Right s -> Right $ apply s t2
                _       -> Left
                           $ Compiling
                           $ "the type of  "
                                ++ show e1 ++ ": "    ++ show t1
                                ++ " does not match " ++ show e2
                                ++ ": "    ++ show t2'
       Left e            -> Left e
       Right t           -> Left
                            $ Compiling
                            $ "tried to apply an argument to the non-function term "
                                ++ show t

typeOfLet :: Identity -> [Identity] -> DeclaredType -> Term -> Term -> StaticEnv -> EitherTyp
typeOfLet x xs t e1 e2 env
    = do  t'       <- pure $ typFromDeclaredType t
          env'     <- pure $ foldArgs xs env t'
          t1       <- typeOf env' e1
          case unify t1 (absTyp t') [] of
            Right s -> typeOf (applyToEnv ((x, apply s t'):env') s) e2 `debug` (show $ applyToEnv ((x, t'):env') s)
            _       -> typesDifferErr e1 t1 e1 (absTyp t') "let"

typeOfLetRec :: Identity -> [Identity] -> DeclaredType -> Term -> Term -> StaticEnv -> EitherTyp
typeOfLetRec x xs t e1 e2 env
    = do  t'       <- pure $ typFromDeclaredType t
          env'     <- pure $ (x, t'):foldArgs xs env t'
          t1       <- typeOf env' e1
          case unify t1 (absTyp t') [] of
            Right _ -> typeOf env' e2
            _       -> typesDifferErr e1 t1 e1 (absTyp t') "let rec"

typeOfLambda :: [Identity] -> DeclaredType -> Term -> StaticEnv -> EitherTyp
typeOfLambda xs t e env
    = let t'       = typFromDeclaredType t
          env'     = foldArgs xs env t'
      in typeOf env' e

typeOfComp :: String -> Term -> StaticEnv -> EitherTyp
typeOfComp p e env
    = do t1 <- typeOf env e
         case unify t1 TInt [] of
            Right _ -> Right $ TInt :-> TBool
            _ -> case unify t1 TFloat [] of
                    Right _ -> Right $ TFloat :-> TBool
                    _       -> case unify t1 TBool [] of
                                    Right _ -> Right $ TBool :-> TBool
                                    _      -> Left
                                              $ Compiling
                                              $ p
                                                ++ ": "
                                                ++ "arguments must be either float or int or bool, not "
                                                ++ show t1

typeOfProp :: String -> Term -> StaticEnv -> EitherTyp
typeOfProp p e env
    = do t1 <- typeOf env e
         case unify t1 TBool [] of
            Right _ -> Right $ TBool :-> TBool
            _       -> Left
                       $ Compiling
                       $ p
                        ++ ": "
                        ++ "arguments must be bool, not "
                        ++ show t1

typeOfArith :: Identity -> Term -> StaticEnv -> EitherTyp
typeOfArith p e env =
    do t <- typeOf env e
       case unify t TInt [] of
            Right _ -> Right $ TInt :-> TInt
            _       ->
                case unify t TFloat [] of
                    Right _ -> Right $ TFloat :-> TFloat
                    _       -> Left
                               $ Compiling
                               $ p
                                    ++ ": "
                                    ++ "arguments must be either float or int, not "
                                    ++ show t

typeOfCons :: Term -> Term -> StaticEnv -> EitherTyp
typeOfCons e1 e2 env
    = do t1 <- typeOf env e1
         t2 <- typeOf env e2
         case t2 of
            TList t2' -> case unify t1 t2' [] of
                            Right s ->
                                let t = apply s t1 in
                                Right $ TList t
                            _       -> typesDifferErr e1 t1 e2 t2 "list"
            _         -> Left
                         $ Compiling
                         $ "cons: the second argument must be a list not "
                            ++ show t2

foldArgs :: [Identity] -> StaticEnv -> Typ -> StaticEnv
foldArgs (x:xs) env (t1 :-> t2)
    = (x, t1):foldArgs xs env t2
foldArgs _ env _ = env

typesDifferErr :: Term -> Typ -> Term -> Typ -> String -> EitherTyp
typesDifferErr e1 t1 e2 t2 cont
    = Left
    $ Compiling
    $ cont ++ ": the expression "
           ++ show e1 ++ " with type "
           ++ show t1 ++ " does not match "
           ++ show e2 ++ " with type "
           ++ show t2
