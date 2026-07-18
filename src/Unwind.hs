module Unwind where

import STG

-- | Function that unwinds every application until it reaches the leftmost combinator.
unwind :: [Combinator] -> [Combinator]
unwind c
    = foldl unwind' [] c
        where unwind' acc (e1 ::@ e2) = acc ++ unwind' [] e1 ++ unwind' [] e2
              unwind' acc c'          = acc ++ [c']
