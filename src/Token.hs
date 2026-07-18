module Token where

data Token = TokenFix
           | TokenLet
           | TokenRec
           | TokenIf
           | TokenThen
           | TokenElse
           | TokenFn
           | TokenIn
           | TokenTrue
           | TokenFalse
           | TokenAssign
           | TokenPoints
           | TokenArrow
           | TokenPlus
           | TokenMinus
           | TokenTimes
           | TokenDiv
           | TokenLPar
           | TokenRPar
           | TokenMore
           | TokenMoreEqual
           | TokenLess
           | TokenLessEqual
           | TokenEquals
           | TokenDifferent
           | TokenAnd
           | TokenOr
           | TokenLParRect
           | TokenRParRect
           | TokenComma
           | TokenInt Int
           | TokenFloat Float
           | TokenVar String
           | TokenSym String
           deriving (Show, Eq)
