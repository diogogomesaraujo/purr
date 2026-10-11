-- Based on Simon Peyton Jones' tutorial.

module STG where

import Utils
import Ast
import Err (Err (Compiling))
import Data.Map as M

data IConstant = IInt     Int
                 | IFloat Float
                 | IBool  Bool
                 deriving (Show, Eq)

data Instruction = Unwind
                 | PushGlobal Identity
                 | PushConst  IConstant
                 | Push       Int
                 | Mkap
                 | Slide      Int
                 | Alloc      Int
                 | Update     Int
                 | Pop        Int
                 | Eval
                 | Cond Program Program
                 | Add | Sub | Mul | Div
                 | Eq | Diff | Lt | LtEq | Gt | GtEq
                 | And | Or
                 | Cons | Nil
                 | Print
                 deriving (Show, Eq)

type Program = [Instruction]

type CompilerEnv = Map Identity Int

type CompiledSuperCombinators = [Compiled]

type CompilerResult = Either Err (Program, CompiledSuperCombinators, Int)

type Compiler = Term -> CompilerEnv -> Int -> CompilerResult

type Compiled = (Identity, Int, Program)

type CompiledResult = Either Err (Compiled, CompiledSuperCombinators, Int)

initialCode :: Program
initialCode = [PushGlobal "main", Eval]

compiledPrims :: [Compiled]
compiledPrims = [ compiledPrim "+" Add, compiledPrim "-" Sub
                , compiledPrim "*" Mul, compiledPrim "/" Div
                , compiledPrim "<" Lt, compiledPrim "<=" LtEq
                , compiledPrim ">" Gt, compiledPrim ">=" GtEq
                , compiledPrim "==" Eq, compiledPrim "!=" Diff
                , compiledIf ] -- missing :, &&, ||

compiledIf :: Compiled
compiledIf = ("if", 3, [ Push 0, Eval, Cond [Push 1] [Push 2]
                       , Update 3, Pop 3, Unwind])

compiledPrim :: Identity -> Instruction -> Compiled
compiledPrim prim inst
    = (prim, 2, [ Push 1, Eval, Push 1, Eval,
                  inst, Update 2, Pop 2, Unwind ])

compileR :: Compiler
compileR e env i
    = do (compiled, cs, i') <- compile e env i
         return $ (compiled
                  ++ [ Slide (M.size env + 1)
                     , Unwind ], cs, i')

compileSc :: Int -> (Identity, [Identity], Term) -> CompiledResult
compileSc i (name, env, body)
    = do (compiled, cs, i') <- compileR body (enum env) i
         return $ ((name, length env, compiled), cs, i')
            where enum e = M.fromList $ zip e [0..]

compile :: Compiler
compile (Var v) env i
    = compileVar v env i
compile (Const c) env i
    = compileConst c env i
compile (Lambda xs e) env i
    = compileLambda xs e env i
compile (TypedLambda xs _ e) env i
    = compileLambda xs e env i
compile (e1 :@ e2) env i
    = compileAp e1 e2 env i
compile (If e1 e2 e3) env i
    = compileIf e1 e2 e3 env i
compile (Let x xs e1 e2) env i
    = compileLet x (replaceArgs xs e1) e2 env i
compile (TypedLet x xs _ e1 e2) env i
    = compileLet x (replaceArgs xs e1) e2 env i
compile (LetRec x xs e1 e2) env i
    = compileLetRec x (replaceArgs xs e1) e2 env i
compile (TypedLetRec x xs _ e1 e2) env i
    = compileLetRec x (replaceArgs xs e1) e2 env i
compile (Prim p) _ i
    = compilePrim p i
compile (Fix _) _ _
    = Left $ Compiling "shouldn't happen"

compileVar :: Identity -> CompilerEnv -> Int -> CompilerResult
compileVar v env i
    = let res = case M.lookup v env of
                    Just n  -> Push n
                    Nothing -> PushGlobal v in
      return ([res], [], i)

compileConst :: Constant -> CompilerEnv -> Int -> CompilerResult
compileConst (CInt i) _ i'
    = return
      ([PushConst
          $ IInt i], [], i')
compileConst (CFloat f) _ i
    = return
      ([PushConst
          $ IFloat f], [], i)
compileConst (CBool b) _ i
    = return
      ([PushConst
          $ IBool b], [], i)
compileConst (CList l) env i-- TODO
    = compileList l env i

compileList :: [Term] -> CompilerEnv -> Int -> CompilerResult
compileList [] _ i
    = return ([Nil], [], i)
compileList (x:xs) env i
    = do (x', x_cs, i_x)    <- compile x env i
         (xs', xs_cs, i_xs) <- compileList xs env i_x
         return $ (x' ++ xs' ++ [Cons], x_cs ++ xs_cs, i_xs)

compileLambda :: [Identity] -> Term -> CompilerEnv -> Int -> CompilerResult
compileLambda xs e _ i
    = let (name, i')    = runFreshVariable lambdaFreshVariable i in
      do  (sc, cs, i'') <- compileSc i' (name, xs, e)
          return ([PushGlobal name], sc:cs, i'')

compileAp :: Term -> Term -> CompilerEnv -> Int -> CompilerResult
compileAp e1 e2 env i
    = do (e1', cs1, i1) <- compile e1 env i
         (e2', cs2, i2) <- compile e2 (argsOffset 1 env) i1
         return $ (e1'
                  ++ e2'
                  ++ [Mkap], cs1 ++ cs2, i2)

compileIf :: Term -> Term -> Term -> CompilerEnv -> Int -> CompilerResult
compileIf e1 e2 e3 env i
    = do (e1', cs1, i1) <- compile e1 env i
         (e2', cs2, i2) <- compile e2 env i1
         (e3', cs3, i3) <- compile e3 env i2
         return $ (e1' ++ [Eval, Cond e2' e3'], cs1 ++ cs2 ++ cs3, i3)

compileLet :: Identity -> Term -> Term -> CompilerEnv -> Int -> CompilerResult
compileLet x e1 e2 env i
    = do (e1', cs1, i1)  <- compile e1 env i
         env'            <- return $ M.insert x 0 (argsOffset 1 env)
         (e2', cs2, i2)  <- compile e2 env' i1
         return $ (e1'
                  ++ e2'
                  ++ [Slide 1], cs1 ++ cs2, i2)

compileLetRec :: Identity -> Term -> Term -> CompilerEnv -> Int -> CompilerResult
compileLetRec x e1 e2 env i
    = do env'        <- return $ M.insert x 0 (argsOffset 1 env)
         (e1', cs1, i1)  <- compile e1 env' i
         (e2', cs2, i2)  <- compile e2 env' i1
         return $ ([Alloc 1]
                  ++ e1'
                  ++ [Update 0]
                  ++ e2'
                  ++ [Slide 1], cs1 ++ cs2, i2)

compilePrim :: Operation -> Int -> CompilerResult
compilePrim p i = return ([PushGlobal $ showOp p], [], i)

replaceArgs :: [Identity] -> Term -> Term
replaceArgs [] e = e
replaceArgs (x:xs) e
    = Lambda [x] $ replaceArgs xs e

argsOffset :: Int -> CompilerEnv -> CompilerEnv
argsOffset n env
    = M.map (\m -> n + m) env
