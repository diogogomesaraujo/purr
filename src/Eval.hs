module Eval where

import G
import Rewrite
import Unwind
import Ast

-- | Function that reduces a sequence of combinators until it reaches weak head normal
-- form (WHNF).
eval :: Spine -> Spine
eval e
    = step $ unwind e

-- | Function that does an intermediate step of evaluation (conditions and primitive operations).
step :: Spine -> Spine

step (STGVar "cons":e1:e2:xs)
    = CAT:e1:e2:xs

step (STGVar "nil":xs)
    = (STGConst $ CList []):xs

step (ADD:(STGConst (CInt x)):(STGConst (CInt y)):xs)
    = (STGConst $ CInt (x + y)):xs
step (ADD:(STGConst (CFloat x)):(STGConst (CFloat y)):xs)
    = (STGConst $ CFloat (x + y)):xs

step (ADD:e1:e2:xs)
    = eval $ [ADD] ++ (eval $ unwind [e1]) ++ (eval $ unwind [e2]) ++ xs

step (MUL:(STGConst (CInt x)):(STGConst (CInt y)):xs)
    = (STGConst $ CInt (x * y)):xs
step (MUL:(STGConst (CFloat x)):(STGConst (CFloat y)):xs)
    = (STGConst $ CFloat (x * y)):xs

step (MUL:e1:e2:xs)
    = eval $ [MUL] ++ (eval $ unwind [e1]) ++ (eval $ unwind [e2]) ++ xs

step (SUB:(STGConst (CInt x)):(STGConst (CInt y)):xs)
    = (STGConst $ CInt (x - y)):xs
step  (SUB:(STGConst (CFloat x)):(STGConst (CFloat y)):xs)
    = (STGConst $ CFloat (x - y)):xs

step (SUB:e1:e2:xs)
    = eval $ [SUB] ++ (eval $ unwind [e1]) ++ (eval $ unwind [e2]) ++ xs

step (DIV:(STGConst (CInt x)):(STGConst (CInt y)):xs)
    = (STGConst $ CInt (x `div` y)):xs
step (DIV:(STGConst (CFloat x)):(STGConst (CFloat y)):xs)
    = (STGConst $ CFloat (x / y)):xs

step (DIV:e1:e2:xs)
    = eval $ [DIV] ++ (eval $ unwind [e1]) ++ (eval $ unwind [e2]) ++ xs

step (AND:(STGConst (CBool x)):(STGConst (CBool y)):xs)
    = (STGConst $ CBool (x && y)):xs
step (OR:(STGConst (CBool x)):(STGConst (CBool y)):xs)
    = (STGConst $ CBool (x || y)):xs

step (AND:e1:e2:xs)
    = eval $ [AND] ++ (eval $ unwind [e1]) ++ (eval $ unwind [e2]) ++ xs
step (OR:e1:e2:xs)
    = eval $ [OR] ++ (eval $ unwind [e1]) ++ (eval $ unwind [e2]) ++ xs

step (G.EQ:(STGConst (CBool x)):(STGConst (CBool y)):xs)
    = (STGConst $ CBool (x == y)):xs
step (DIFF:(STGConst (CBool x)):(STGConst (CBool y)):xs)
    = (STGConst $ CBool (x /= y)):xs
step (G.EQ:(STGConst (CFloat x)):(STGConst (CFloat y)):xs)
    = (STGConst $ CBool (x == y)):xs
step (DIFF:(STGConst (CFloat x)):(STGConst (CFloat y)):xs)
    = (STGConst $ CBool (x /= y)):xs
step (G.EQ:(STGConst (CInt x)):(STGConst (CInt y)):xs)
    = (STGConst $ CBool (x == y)):xs
step (DIFF:(STGConst (CInt x)):(STGConst (CInt y)):xs)
    = (STGConst $ CBool (x /= y)):xs

step (G.EQ:e1:e2:xs)
    = eval $ [G.EQ] ++ (eval $ unwind [e1]) ++ (eval $ unwind [e2]) ++ xs
step (DIFF:e1:e2:xs)
    = eval $ [DIFF] ++ (eval $ unwind [e1]) ++ (eval $ unwind [e2]) ++ xs

step (L:(STGConst (CInt x)):(STGConst (CInt y)):xs)
    = (STGConst $ CBool (x < y)):xs
step (LE:(STGConst (CInt x)):(STGConst (CInt y)):xs)
    = (STGConst $ CBool (x <= y)):xs
step (L:(STGConst (CFloat x)):(STGConst (CFloat y)):xs)
    = (STGConst $ CBool (x < y)):xs
step (LE:(STGConst (CFloat x)):(STGConst (CFloat y)):xs)
    = (STGConst $ CBool (x <= y)):xs

step (L:e1:e2:xs)
    = eval $ [L] ++ (eval $ unwind [e1]) ++ (eval $ unwind [e2]) ++ xs
step (LE:e1:e2:xs)
    = eval $ [LE] ++ (eval $ unwind [e1]) ++ (eval $ unwind [e2]) ++ xs

step (G:(STGConst (CFloat x)):(STGConst (CFloat y)):xs)
    = (STGConst $ CBool (x > y)):xs
step (GE:(STGConst (CFloat x)):(STGConst (CFloat y)):xs)
    = (STGConst $ CBool (x >= y)):xs
step (G:(STGConst (CInt x)):(STGConst (CInt y)):xs)
    = (STGConst $ CBool (x > y)):xs
step (GE:(STGConst (CInt x)):(STGConst (CInt y)):xs)
    = (STGConst $ CBool (x >= y)):xs

step (G:e1:e2:xs)
    = eval $ [G] ++ (eval $ unwind [e1]) ++ (eval $ unwind [e2]) ++ xs
step (GE:e1:e2:xs)
    = eval $ [GE] ++ (eval $ unwind [e1]) ++ (eval $ unwind [e2]) ++ xs

step (CAT:(STGConst c):(STGConst (CList l)):xs)
    = eval $ (STGConst $ CList $ c:l):xs
step (CAT:e1:e2:xs)
    = eval $ [CAT] ++ (eval $ unwind [e1]) ++ (eval $ unwind [e2]) ++ xs

step (STGIf:(STGConst (CBool x)):e1:e2:xs)
    | x         = eval $ e1:xs
    | otherwise = eval $ e2:xs

step (STGIf:e1:e2:e3:xs)
    = eval $ [STGIf] ++ (eval $ unwind [e1]) ++ e2:e3:xs

step e
    | e == rewrite e = e
    | otherwise      = eval $ rewrite e
