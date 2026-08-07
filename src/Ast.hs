module Ast where
import Name

type Identity = String

data Constant = CInt   Int
              | CFloat Float
              | CBool  Bool
              | CList [Term]
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
               | (:::)
               | Custom String
               deriving (Show, Eq)

data Term = Var         Identity
          | Const       Constant
          | Lambda      [Identity] Term
          | TypedLambda [Identity] DeclaredType Term
          | Term :@ Term
          | If          Term Term Term
          | Let         Identity [Identity] Term Term
          | TypedLet    Identity [Identity] DeclaredType Term Term
          | LetRec      Identity [Identity] Term Term
          | TypedLetRec Identity [Identity] DeclaredType Term Term
          | Fix         Term
          | Prim        Operation
          deriving (Show, Eq)

data DeclaredType = DVar Identity
                  | DList DeclaredType
                  | DeclaredType ::-> DeclaredType
                  deriving (Show, Eq)

cons :: Term -> Term -> Term
cons x y = (Var "cons" :@ x) :@ y

nil :: Term
nil = Var "nil"

makeBalelasArgs :: Term -> [Identity] -> (Term, [Identity])
makeBalelasArgs (Var _) ns
    = let n = newName ns in (Var n, n:ns)
makeBalelasArgs _ _ = undefined
