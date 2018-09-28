module EvolutionConfig (module EvolutionConfig) where

import EvolutionTypes
import Data.Map
import Data.List
import Data.Ord

mutateChildChance = 0.1
mutateLeafChance = 0.1
mutateFuncChance = 0.1

charsSol = ['x']
charsTU = ['a', 'b', 'c']--, 'd', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm']
numAlternatives = 1000 :: Int
treeDepth = 4

--defaultEnv :: Env
--defaultEnv = fromList [('x', 1.0), ('y', 2.0)]

availableFunctions = [Add,Sub,Mul]
