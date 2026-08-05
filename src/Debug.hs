module Debug where

import Debug.Trace

debug :: c -> String -> c
debug = flip trace
