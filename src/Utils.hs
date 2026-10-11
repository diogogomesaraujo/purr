module Utils where

type Identity = String

type Addr = Int

newtype FreshVariable = FreshVariable (Int -> (String, Int))

runFreshVariable :: FreshVariable -> Int -> (String, Int)
runFreshVariable (FreshVariable f) i = f i

lambdaFreshVariable :: FreshVariable
lambdaFreshVariable
    = FreshVariable $
        \i -> ("$lambda" ++ show i, i + 1)
