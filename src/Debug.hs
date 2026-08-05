module Debug where

import Debug.Trace

-- | Function to help debugging (to use do ... `debug` what you want to debug).
debug :: c -> String -> c
debug = flip trace
