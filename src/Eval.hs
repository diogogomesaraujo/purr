module Eval where

import STG
import Normal
import Unwind
import Rewrite

-- | Function that reduces a sequence of combinators until it reaches weak head normal
-- form (WHNF).
eval :: [Combinator] -> [Combinator]
eval c
    | normal c
        = c
    | otherwise
        = let c' = unwind c in
          eval $ rewrite c'
