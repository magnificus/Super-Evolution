module EvolutionConfig (module EvolutionConfig) where

import EvolutionTypes
import Data.Map
import Data.List
import Data.Ord

mutateChildChance = 0.03
mutateLeafChance = 0.03
mutateFuncChance = 0.03
changeNumberChance = 0.03
changeVariableChance = 0.02
changeNumberRange = (0.5, 1.5)
cullRatio = 0.10 -- values greater than 0.5 will give strange results

charsSol = ["x", "y"]
availableTU = [1..100] -- the maximum number of genes
numAlternatives = 500 :: Int
treeDepth = 5

availableFunctions = [Add,Sub,Mul, Div, Pow ,Log, LeftOnly, RightOnly]
