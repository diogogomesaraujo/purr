module Name where

import Data.List

-- | Function that gives an unused name to an arbitrary type (used in typeTerm).s
newName :: [String] -> String
newName l
    = findAvailableName $ combinations alphabet \\ [""]
        where findAvailableName (x:xs)
                  = if x `notElem` l
                        then x
                        else findAvailableName xs
              findAvailableName _ = error "impossible"

-- | Function that gives all combinations of the elements in a list (percs of using Haskell xD).
combinations :: [a] -> [[a]]
combinations [] = [[]]
combinations (x:xs)
    =  combinations xs ++ map (x:)(combinations xs)

-- | Constant list of all letters in the alphabet.
alphabet :: [Char]
alphabet = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm'
           , 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z']
