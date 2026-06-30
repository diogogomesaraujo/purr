module Ast where

type Identity = String

data Constant = CInt   Int
              | CFloat Float
              | CBool  Bool
              deriving Show

data Term = Var    Identity
          | Const  Constant
          | Lambda Identity Term
          | App    Term Term
          | If     Term Term Term
          | Let    Identity Term Term
          | Fix    Term
          | Term :+  Term
          | Term :-  Term
          | Term :/  Term
          | Term :*  Term
          | Term :&& Term
          | Term :|| Term
          | Term :>  Term
          | Term :>= Term
          | Term :<  Term
          | Term :<= Term
          | Term :== Term
          | Term :!= Term
          deriving Show
