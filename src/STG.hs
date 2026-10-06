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
                 deriving (Show, Eq)

type Program = [Instruction]

type CompilerEnv = Map Identity Int

type CompilerResult = Either Err Program

type Compiler = Term -> CompilerEnv -> CompilerResult

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
compileAp = undefined
compileIf = undefined
compileLet = undefined
compileLetRec = undefined
compilePrim = undefined

replaceArgs :: [Identity] -> Term -> Term
replaceArgs [] e = e
replaceArgs (x:xs) e
    = Lambda [x] $ replaceArgs xs e
