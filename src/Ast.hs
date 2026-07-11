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
          | Let    Identity [Identity] Term Term
          | LetRec Identity [Identity] Term Term
          | LetOp  Identity Identity Identity Term Term
          | Fix    Term
          | Prim   Operation
          | Lst    [Term]
          deriving (Show, Eq)
