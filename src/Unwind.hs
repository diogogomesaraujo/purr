module Unwind where

import G

type Spine = [Combinator]

-- | Function that unwinds applications.
unwind :: Spine -> Spine
unwind (e1 ::@ e2:xs)
    = unwind (e1:e2:xs)
unwind e = e
