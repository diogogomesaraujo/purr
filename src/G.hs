module G where

import Ast
import Data.List

data STGConstant = STGInt   Int
                 | STGFloat Float
                 | STGBool  Bool
                 | STGList [Combinator]
                 deriving (Show, Eq)

data Combinator = STGVar Identity
                | Combinator ::@ Combinator -- Application
                | S | K | I | B | C | Y
                | ADD | MUL | SUB | DIV
                | AND | OR | EQ | DIFF
                | GE | G | LE | L
                | CONS | HEAD | TAIL
                | STGConst STGConstant
                | STGIf
                deriving (Show, Eq)

cons :: Combinator
cons = STGVar "cons"

maybeShowConst :: Combinator -> Maybe String
maybeShowConst (STGConst (STGBool b))  = pure $ show b
maybeShowConst (STGConst (STGInt i))   = pure $ show i
maybeShowConst (STGConst (STGFloat f)) = pure $ show f
maybeShowConst (STGConst (STGList l))
    = (\xs -> "[" ++ intercalate "," xs ++ "]")
        <$> traverse maybeShowConst l
maybeShowConst _ = Nothing

-- | Function that returns the free variables in a term.
fv :: Term -> [Identity]
fv (Var x)               = [x]
fv (Lambda xs e)         = (fv e) \\ xs
fv (e1 :@ e2)            = fv e1 `union` fv e2
fv _                     = []

-- | Function that checks if there are any free variables in a term.
isFv :: Identity -> Combinator -> Bool
isFv v (STGVar x)  = v == x
isFv v (e1 ::@ e2) = isFv v e1 || isFv v e2
isFv _ _           = False
