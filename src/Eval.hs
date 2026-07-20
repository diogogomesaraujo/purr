module Eval where

import STG
import Normal
import Rewrite
import Data.Generics.Uniplate.Data (descend)

-- | Function that reduces a sequence of combinators until it reaches weak head normal
-- form (WHNF).
eval :: Combinator -> Combinator
eval c
    | normal c
        = let c' = descend eval c in
            if normal c'
                then c'
                else eval c'
    | otherwise  = eval $ rewrite c
