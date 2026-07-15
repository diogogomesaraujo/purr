module Unwind where

import STG

unwind :: [Combinator] -> [Combinator]
unwind c
    = foldl unwind' [] c
        where unwind' acc (e1 ::@ e2) = acc ++ [e1] ++ [e2]
              unwind' _ c'            = [c']
