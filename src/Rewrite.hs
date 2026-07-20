module Rewrite where

import STG
import Ast

-- | Function that tries to reduce sequences of combinators.
rewrite :: Combinator -> Combinator

rewrite (I ::@ p)
    = p

rewrite (K ::@ p ::@ _)
    = p

rewrite (S ::@ p ::@ q ::@ r)
    = (p ::@ r) ::@ (q ::@ r)

rewrite (B ::@ p ::@ q ::@ r)
    = (p ::@ (q ::@ r))

rewrite (C ::@ p ::@ q ::@ r)
    = ((p ::@ r) ::@ q)

rewrite (Y ::@ p)
    = (p ::@ (Y ::@ p))

rewrite (ADD ::@ (STGConst (CInt x)) ::@ (STGConst (CInt y)))
    = (STGConst $ CInt (x + y))
rewrite (ADD ::@ (STGConst (CFloat x)) ::@ (STGConst (CFloat y)))
    = (STGConst $ CFloat (x + y))

rewrite (MUL ::@ (STGConst (CInt x)) ::@ (STGConst (CInt y)))
    = (STGConst $ CInt (x * y))
rewrite (MUL ::@ (STGConst (CFloat x)) ::@ (STGConst (CFloat y)))
    = (STGConst $ CFloat (x * y))

rewrite (SUB ::@ (STGConst (CInt x)) ::@ (STGConst (CInt y)))
    = (STGConst $ CInt (x - y))
rewrite (SUB ::@ (STGConst (CFloat x)) ::@ (STGConst (CFloat y)))
    = (STGConst $ CFloat (x - y))

rewrite (DIV ::@ (STGConst (CInt x)) ::@ (STGConst (CInt y)))
    = (STGConst $ CInt (x `div` y))
rewrite (DIV ::@ (STGConst (CFloat x)) ::@ (STGConst (CFloat y)))
    = (STGConst $ CFloat (x / y))

rewrite (AND ::@ (STGConst (CBool x)) ::@ (STGConst (CBool y)))
    = (STGConst $ CBool (x && y))
rewrite (OR ::@ (STGConst (CBool x)) ::@ (STGConst (CBool y)))
    = (STGConst $ CBool (x || y))

rewrite (STG.EQ ::@ (STGConst (CBool x)) ::@ (STGConst (CBool y)))
    = (STGConst $ CBool (x == y))
rewrite (DIFF ::@ (STGConst (CBool x)) ::@ (STGConst (CBool y)))
    = (STGConst $ CBool (x /= y))
rewrite (STG.EQ ::@ (STGConst (CFloat x)) ::@ (STGConst (CFloat y)))
    = (STGConst $ CBool (x == y))
rewrite (DIFF ::@ (STGConst (CFloat x)) ::@ (STGConst (CFloat y)))
    = (STGConst $ CBool (x /= y))
rewrite (STG.EQ ::@ (STGConst (CInt x)) ::@ (STGConst (CInt y)))
    = (STGConst $ CBool (x == y))
rewrite (DIFF ::@ (STGConst (CInt x)) ::@ (STGConst (CInt y)))
    = (STGConst $ CBool (x /= y))

rewrite (L ::@ (STGConst (CInt x)) ::@ (STGConst (CInt y)))
    = (STGConst $ CBool (x < y))
rewrite (LE ::@ (STGConst (CInt x)) ::@ (STGConst (CInt y)))
    = (STGConst $ CBool (x <= y))
rewrite (L ::@ (STGConst (CFloat x)) ::@ (STGConst (CFloat y)))
    = (STGConst $ CBool (x < y))
rewrite (LE ::@ (STGConst (CFloat x)) ::@ (STGConst (CFloat y)))
    = (STGConst $ CBool (x <= y))

rewrite (G ::@ (STGConst (CFloat x)) ::@ (STGConst (CFloat y)))
    = (STGConst $ CBool (x > y))
rewrite (GE ::@ (STGConst (CFloat x)) ::@ (STGConst (CFloat y)))
    = (STGConst $ CBool (x >= y))
rewrite (G ::@ (STGConst (CInt x)) ::@ (STGConst (CInt y)))
    = (STGConst $ CBool (x > y))
rewrite (GE ::@ (STGConst (CInt x)) ::@ (STGConst (CInt y)))
    = (STGConst $ CBool (x >= y))

rewrite (STGIf ::@ STGConst (CBool x) ::@ e1 ::@ e2)
    | x         = e1
    | otherwise = e2

rewrite t
    = t
