module Rewrite where

import STG

-- | Function that tries to reduce sequences of combinators.
rewrite :: [Combinator] -> [Combinator]

rewrite (I:p:tl)
    = p:tl

rewrite (K:p:_:tl)
    = p:tl

rewrite (S:p:q:r:tl)
    = p:r:(q ::@ r):tl

rewrite (B:p:q:r:tl)
    = p:(q ::@ r):tl

rewrite (C:p:q:r:tl)
    = (p ::@ r):q:tl

rewrite t
    = t
