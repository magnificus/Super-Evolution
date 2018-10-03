module EvolutionConfig (module EvolutionConfig) where

import EvolutionTypes
import Data.Map
import Data.List
import Data.Ord

mutateChildChance = 0.05
mutateLeafChance = 0.05
mutateFuncChance = 0.05
changeNumberChance = 0.05
changeVariableChance = 0.05
changeNumberRange = (0.5, 1.5)
cullRatio = 0.15

charsSol = ['x']
charsTU = ['a', 'b', 'c', 'd', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm']
numAlternatives = 100 :: Int
treeDepth = 4

availableFunctions = [Add,Sub,Mul]
