module Unwind where

import G

type Spine = [Combinator]

unwind :: Spine -> Spine
unwind (e1 ::@ e2:xs)
    = unwind (e1:e2:xs)
unwind e = e
