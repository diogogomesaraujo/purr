module Eval where

import STG
import Normal
import Unwind
import Rewrite

eval :: [Combinator] -> [Combinator]
eval c
    | normal c
        = c
    | otherwise
        = let c' = unwind c in
          eval $ rewrite c'
