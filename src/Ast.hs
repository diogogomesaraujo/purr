module Ast where

type Identity = String

data Constant = CInt   Int
              | CFloat Float
              | CBool  Bool
              deriving (Show, Eq)

data Operation = (:+)
               | (:-)
               | (:/)
               | (:*)
               | (:&&)
               | (:||)
               | (:>)
               | (:>=)
               | (:<)
               | (:<=)
               | (:==)
               | (:!=)
               | Custom String
               deriving (Show, Eq)

data Term = Var    Identity
          | Const  Constant
          | Lambda Identity Term
          | App    Term Term
          | If     Term Term Term
          | Let    Identity Term Term
          | Fix    Term
          | Prim   Operation
          deriving (Show, Eq)
