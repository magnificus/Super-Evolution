module EvolutionConfig (module EvolutionConfig) where

import EvolutionTypes
import Data.Map
import Data.List
import Data.Ord

mutateChildChance = 0.03
mutateLeafChance = 0.03
mutateFuncChance = 0.03
changeNumberChance = 0.07
changeVariableChance = 0.02
changeNumberRange = (0.5, 1.5)
cullRatio = 0.15 -- values greater than 0.5 will give strange results

charsSol = ['x']
availableTU = [1..20] -- the maximum number of genes
numAlternatives = 300 :: Int
treeDepth = 4

availableFunctions = [Add,Sub,Mul, Pow, Div, Log]
