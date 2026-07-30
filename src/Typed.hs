module Typed where

data Typ = TBool
         | TInt
         | TFloat
         | Typ :-> Typ
         deriving (Show, Eq)

infixr 4 :->
