module Fv where

import Ast
import Typed
import Data.Set as S
import Data.List as L

fvMono :: Mono -> Set Identity
fvMono (TVar v)
    = S.singleton v
fvMono (TApp _ ts)
    = L.foldl (\acc t ->
        S.union (fvMono t) acc) empty ts

fvPoly :: Poly -> Set Identity
fvPoly (vs, t) =
    (S.\\) (fvMono t) vs
