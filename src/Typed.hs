module Typed where

import Ast
import Data.Set
import Data.Map

type AFunc = Identity
type AVar  = Identity

data Mono = TVar Identity
          | TApp AFunc [Mono]
          deriving (Show, Eq)

type Poly = (Set AVar, Mono)

type Env = Map AVar Mono
