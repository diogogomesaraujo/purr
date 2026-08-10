module Eval where

import G
import Rewrite
import Unwind

-- | Function that reduces a sequence of combinators until it reaches weak head normal
-- form (WHNF).
eval :: Spine -> Spine
eval e
    = step $ unwind e

-- | Function that does an intermediate step of evaluation (conditions and primitive operations).
step :: Spine -> Spine

step (STGVar "cons":e1:e2:xs)
    = CONS:e1:e2:xs

step (STGVar "nil":xs)
    = (STGConst $ STGList []):xs

step (ADD:(STGConst (STGInt x)):(STGConst (STGInt y)):xs)
    = (STGConst $ STGInt (x + y)):xs
step (ADD:(STGConst (STGFloat x)):(STGConst (STGFloat y)):xs)
    = (STGConst $ STGFloat (x + y)):xs

step (ADD:e1:e2:xs)
    = eval $ [ADD] ++ (eval $ unwind [e1]) ++ (eval $ unwind [e2]) ++ xs

step (MUL:(STGConst (STGInt x)):(STGConst (STGInt y)):xs)
    = (STGConst $ STGInt (x * y)):xs
step (MUL:(STGConst (STGFloat x)):(STGConst (STGFloat y)):xs)
    = (STGConst $ STGFloat (x * y)):xs

step (MUL:e1:e2:xs)
    = eval $ [MUL] ++ (eval $ unwind [e1]) ++ (eval $ unwind [e2]) ++ xs

step (SUB:(STGConst (STGInt x)):(STGConst (STGInt y)):xs)
    = (STGConst $ STGInt (x - y)):xs
step  (SUB:(STGConst (STGFloat x)):(STGConst (STGFloat y)):xs)
    = (STGConst $ STGFloat (x - y)):xs

step (SUB:e1:e2:xs)
    = eval $ [SUB] ++ (eval $ unwind [e1]) ++ (eval $ unwind [e2]) ++ xs

step (DIV:(STGConst (STGInt x)):(STGConst (STGInt y)):xs)
    = (STGConst $ STGInt (x `div` y)):xs
step (DIV:(STGConst (STGFloat x)):(STGConst (STGFloat y)):xs)
    = (STGConst $ STGFloat (x / y)):xs

step (DIV:e1:e2:xs)
    = eval $ [DIV] ++ (eval $ unwind [e1]) ++ (eval $ unwind [e2]) ++ xs

step (AND:(STGConst (STGBool x)):(STGConst (STGBool y)):xs)
    = (STGConst $ STGBool (x && y)):xs
step (OR:(STGConst (STGBool x)):(STGConst (STGBool y)):xs)
    = (STGConst $ STGBool (x || y)):xs

step (AND:e1:e2:xs)
    = eval $ [AND] ++ (eval $ unwind [e1]) ++ (eval $ unwind [e2]) ++ xs
step (OR:e1:e2:xs)
    = eval $ [OR] ++ (eval $ unwind [e1]) ++ (eval $ unwind [e2]) ++ xs

step (G.EQ:(STGConst (STGBool x)):(STGConst (STGBool y)):xs)
    = (STGConst $ STGBool (x == y)):xs
step (DIFF:(STGConst (STGBool x)):(STGConst (STGBool y)):xs)
    = (STGConst $ STGBool (x /= y)):xs
step (G.EQ:(STGConst (STGFloat x)):(STGConst (STGFloat y)):xs)
    = (STGConst $ STGBool (x == y)):xs
step (DIFF:(STGConst (STGFloat x)):(STGConst (STGFloat y)):xs)
    = (STGConst $ STGBool (x /= y)):xs
step (G.EQ:(STGConst (STGInt x)):(STGConst (STGInt y)):xs)
    = (STGConst $ STGBool (x == y)):xs
step (DIFF:(STGConst (STGInt x)):(STGConst (STGInt y)):xs)
    = (STGConst $ STGBool (x /= y)):xs

step (G.EQ:(STGConst (STGList (lx:lxs))):(STGConst (STGList (ly:lys))):xs)
    = step $ AND:(G.EQ ::@ lx ::@ ly):(G.EQ ::@ STGConst (STGList lxs) ::@ STGConst (STGList lys)):xs
step (DIFF:(STGConst (STGList (lx:lxs))):(STGConst (STGList (ly:lys))):xs)
    = step $ AND:(DIFF ::@ lx ::@ ly):(DIFF ::@ STGConst (STGList lxs) ::@ STGConst (STGList lys)):xs
step (DIFF:(STGConst (STGList [])):(STGConst (STGList [])):xs)
    = STGConst (STGBool False):xs
step (G.EQ:(STGConst (STGList [])):(STGConst (STGList [])):xs)
    = STGConst (STGBool True):xs
step (G.EQ:(STGConst (STGList _)):(STGConst (STGList _)):xs)
    = STGConst (STGBool False):xs
step (DIFF:(STGConst (STGList _)):(STGConst (STGList _)):xs)
    = STGConst (STGBool True):xs

step (G.EQ:e1:e2:xs)
    = eval $ [G.EQ] ++ (eval $ unwind [e1]) ++ (eval $ unwind [e2]) ++ xs
step (DIFF:e1:e2:xs)
    = eval $ [DIFF] ++ (eval $ unwind [e1]) ++ (eval $ unwind [e2]) ++ xs

step (L:(STGConst (STGInt x)):(STGConst (STGInt y)):xs)
    = (STGConst $ STGBool (x < y)):xs
step (LE:(STGConst (STGInt x)):(STGConst (STGInt y)):xs)
    = (STGConst $ STGBool (x <= y)):xs
step (L:(STGConst (STGFloat x)):(STGConst (STGFloat y)):xs)
    = (STGConst $ STGBool (x < y)):xs
step (LE:(STGConst (STGFloat x)):(STGConst (STGFloat y)):xs)
    = (STGConst $ STGBool (x <= y)):xs

step (L:e1:e2:xs)
    = eval $ [L] ++ (eval $ unwind [e1]) ++ (eval $ unwind [e2]) ++ xs
step (LE:e1:e2:xs)
    = eval $ [LE] ++ (eval $ unwind [e1]) ++ (eval $ unwind [e2]) ++ xs

step (G:(STGConst (STGFloat x)):(STGConst (STGFloat y)):xs)
    = (STGConst $ STGBool (x > y)):xs
step (GE:(STGConst (STGFloat x)):(STGConst (STGFloat y)):xs)
    = (STGConst $ STGBool (x >= y)):xs
step (G:(STGConst (STGInt x)):(STGConst (STGInt y)):xs)
    = (STGConst $ STGBool (x > y)):xs
step (GE:(STGConst (STGInt x)):(STGConst (STGInt y)):xs)
    = (STGConst $ STGBool (x >= y)):xs

step (G:e1:e2:xs)
    = eval $ [G] ++ (eval $ unwind [e1]) ++ (eval $ unwind [e2]) ++ xs
step (GE:e1:e2:xs)
    = eval $ [GE] ++ (eval $ unwind [e1]) ++ (eval $ unwind [e2]) ++ xs

step (CONS:(STGConst c):(STGConst (STGList l)):xs)
    = eval $ (STGConst $ STGList $ (STGConst c):l):xs
step (CONS:e1:e2:xs)
    = eval $ [CONS] ++ (eval $ unwind [e1]) ++ (eval $ unwind [e2]) ++ xs

step (STGIf:(STGConst (STGBool x)):e1:e2:xs)
    | x         = eval $ e1:xs
    | otherwise = eval $ e2:xs

step (STGIf:e1:e2:e3:xs)
    = eval $ [STGIf] ++ (eval $ unwind [e1]) ++ e2:e3:xs

step e
    | e == rewrite e = e
    | otherwise      = eval $ rewrite e
