module EvolutionHelpers (module EvolutionHelpers) where

import System.Random
import EvolutionTypes
import Data.List

randD :: StdGen -> [Double] -- returns an infinite list of random doubles
randD = randomRs (0.0::Double, 0.9999::Double)

lerp (a, b) d = (b-a)*d + a

-- returns an infinite list of randomgenerators from a seed generator
randomGenerators g = Data.List.unfoldr (\g1 -> Just $ System.Random.split g1) g

-- takes a random number between 0 and 1 and a list and returns the element closest to that position in the list
getPositionInList :: Double -> [a] -> a
getPositionInList d l = l !! (floor $ d* (fromIntegral $ length l))

