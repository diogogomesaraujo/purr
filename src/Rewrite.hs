module Rewrite where

import G
import Unwind

-- | Function that tries to reduce sequences of combinators.
rewrite :: Spine -> Spine

rewrite (I:p:xs)
    = p:xs

rewrite (K:p:_:xs)
    = p:xs

rewrite (S:p:q:r:xs)
    = (p ::@ r):(q ::@ r):xs

rewrite (B:p:q:r:xs)
    = p:(q ::@ r):xs

rewrite (C:p:q:r:xs)
    = (p ::@ r):q:xs

rewrite (Y:p:xs)
    = p:(Y ::@ p):xs

rewrite t
    = t
