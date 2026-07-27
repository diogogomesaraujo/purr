module Normal where

import G
import Rewrite

normal :: Combinator -> Bool
normal t = rewrite t == t
