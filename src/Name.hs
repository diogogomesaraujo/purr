module Name where

nameFromCount :: Int -> (String, Int)
nameFromCount i
    = ("_" ++ show i, i + 1)
