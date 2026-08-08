module Stdlib where
import Module (appendModule)

stdPath :: FilePath
stdPath = "stdlib/common.pr"

withStd :: String -> IO String
withStd cont
    = appendModule stdPath cont
