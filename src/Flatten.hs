module Flatten where

import STG

flatten :: Combinator -> [Combinator]
flatten (e1 ::@ e2)
    = flatten e1 ++ flatten e2
flatten e = [e]
