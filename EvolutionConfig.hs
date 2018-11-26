module EvolutionConfig (module EvolutionConfig) where

import EvolutionTypes
import Data.Map
import Data.List
import Data.Ord

mutateChildChance = 0.03
mutateLeafChance = 0.03
mutateFuncChance = 0.03
changeNumberChance = 0.10
changeVariableChance = 0.02
changeNumberRange = (0.5, 1.5)
cullRatio = 0.15 -- values greater than 0.5 will give strange results

availableTU = [1..200] -- the "genes" that each correspond to a translation unit
numAlternatives = 100 :: Int -- the number of organisms to evolve
treeDepth = 4

availableFunctions = [Add,Sub,Mul, Div, Pow ,Log, LeftOnly, RightOnly] -- functions that the trees are allowed to use
