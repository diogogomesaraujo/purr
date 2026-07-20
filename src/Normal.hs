module Normal where

import STG
import Rewrite

normal :: Combinator -> Bool
normal t = rewrite t == t
