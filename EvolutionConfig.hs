module EvolutionConfig (module EvolutionConfig) where

import EvolutionTypes
import Data.Map
import Data.List
import Data.Ord

mutateChildChance = 0.02
mutateLeafChance = 0.02
mutateFuncChance = 0.02
changeNumberChance = 0.05
changeVariableChance = 0.02
changeNumberRange = (0.5, 1.5)
cullRatio = 0.10 -- values greater than 0.5 will give strange results

charsSol = ['x']
availableTU = [1..15] -- the maximum number of genes
numAlternatives = 1000 :: Int
treeDepth = 4

availableFunctions = [Add,Sub,Mul, Pow, Div, Log]
