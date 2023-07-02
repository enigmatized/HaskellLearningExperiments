{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE FlexibleContexts #-}

import Data.List

data Tree   a = a :& Forest a deriving (Eq, Ord, Show, Foldable)
type Forest a = [Tree a]


data Dog a = Int  | Dog a deriving (Eq, Ord, Show, Foldable)



enumerate :: [a] -> [Forest a]
enumerate a  = []


allPerms :: [a] -> [a]
allPerms []  = []
allPerms [a] = [a]
--allPerms xs = (permutations xs) ++ (allPerms $ tail xs)



enumie :: [a] -> b
enumie [] = []
enumie [a] = [[a]]
enumie (x:xs) = [(x:xs)] ++ (map (\y ->[ x] ++[y] )  (enumie xs))


