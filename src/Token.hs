module Token where

data Token = TokenFix
           | TokenLet
           | TokenIf
           | TokenThen
           | TokenElse
           | TokenFn
           | TokenIn
           | TokenTrue
           | TokenFalse
           | TokenAssign
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
           | TokenInt Int
           | TokenFloat Float
           | TokenVar String
           | TokenSym String
           deriving (Show, Eq)
