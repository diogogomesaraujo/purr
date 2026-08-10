module G where

import Ast
import Data.List (intercalate)

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
