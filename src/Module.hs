module Module where

appendModule :: FilePath -> String -> IO String
appendModule path cont
    = do file <- readFile path
         pure $ file ++ " " ++ cont
