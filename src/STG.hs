-- Based on Simon Peyton Jones' tutorial.

module STG where

import Utils
import Ast
import Err (Err)
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
                 | Pack Int Int
                 | CaseJump (Map Int Program)
                 | Split Int
                 | Print
                 deriving (Show, Eq)

type Program = [Instruction]

type CompilerEnv = Map Identity Int

type CompilerResult = Either Err Program

type Compiler = Term -> CompilerEnv -> CompilerResult

type Compiled = (Identity, Int, Program)

type CompiledResult = Either Err Compiled

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
compileR e env
    = do compiled <- compile e env
         return $ compiled
                  ++ [ Slide (M.size env + 1)
                     , Unwind ]

compileSc :: (Identity, [Identity], Term) -> CompiledResult
compileSc (name, env, body)
    = do compiled <- compileR body (enum env)
         return $ (name, length env, compiled)
            where enum e = M.fromList $ zip e [0..]

compile :: Compiler
compile (Var v) env
    = compileVar v env
compile (Const c) env
    = compileConst c env
compile (Lambda xs e) env
    = compileLambda xs e env
compile (TypedLambda xs _ e) env
    = compileLambda xs e env
compile (e1 :@ e2) env
    = compileAp e1 e2 env
compile (If e1 e2 e3) env
    = compileIf e1 e2 e3 env
compile (Let x xs e1 e2) env
    = compileLet x (replaceArgs xs e1) e2 env
compile (TypedLet x xs _ e1 e2) env
    = compileLet x (replaceArgs xs e1) e2 env
compile (LetRec x xs e1 e2) env
    = compileLetRec x (replaceArgs xs e1) e2 env
compile (TypedLetRec x xs _ e1 e2) env
    = compileLetRec x (replaceArgs xs e1) e2 env
compile (Prim p) env
    = compilePrim p env
compile _ _
    = undefined

compileVar :: Identity -> CompilerEnv -> CompilerResult
compileVar v env
    = return
      $ return
      $ case M.lookup v env of
             Just n  -> Push n
             Nothing -> PushGlobal v

compileConst :: Constant -> CompilerEnv -> CompilerResult
compileConst (CInt i) _
    = return
      $ return
      $ PushConst
      $ IInt i
compileConst (CFloat f) _
    = return
      $ return
      $ PushConst
      $ IFloat f
compileConst (CBool b) _
    = return
      $ return
      $ PushConst
      $ IBool b
compileConst (CList _) _
    = undefined

compileLambda = undefined

compileAp :: Term -> Term -> CompilerEnv -> CompilerResult
compileAp e1 e2 env
    = do e1' <- compile e1 env
         e2' <- compile e2 (argsOffset 1 env)
         return $ e1'
                  ++ e2'
                  ++ [Mkap]

compileIf = undefined

compileLet :: Identity -> Term -> Term -> CompilerEnv -> CompilerResult
compileLet x e1 e2 env
    = do e1'  <- compile e1 env
         env' <- return $ M.insert x 0 (argsOffset 1 env)
         e2'  <- compile e2 env'
         return $ e1'
                  ++ e2'
                  ++ [Slide 1]

compileLetRec :: Identity -> Term -> Term -> CompilerEnv -> CompilerResult
compileLetRec x e1 e2 env
    = do e1'  <- compile e1 env
         env' <- return $ M.insert x 0 (argsOffset 1 env)
         e2'  <- compile e2 env'
         return $ [Alloc 1]
                  ++ e1'
                  ++ [Update 0]
                  ++ e2'
                  ++ [Slide 1]

compilePrim = undefined

replaceArgs :: [Identity] -> Term -> Term
replaceArgs [] e = e
replaceArgs (x:xs) e
    = Lambda [x] $ replaceArgs xs e

argsOffset :: Int -> CompilerEnv -> CompilerEnv
argsOffset n env
    = M.map (\m -> n + m) env
