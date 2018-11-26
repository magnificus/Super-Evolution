module Main where

-- copyright Tobias Elinder, see Licence.txt


import System.Random
import Data.Map
import Data.List
import Data.Ord
import Control.Monad
import Data.Maybe

import EvolutionTypes
import EvolutionConfig
import EvolutionHelpers
import EvolutionParser

--this file (together with EvolutionTypes.hs) contains the base logic for the evolutionary structure etc.

altToTree :: Alternative -> Tree
altToTree a = toTree (alternativeTop a) a treeDepth

getLeaf :: [String] -> [Double] -> Leaf
getLeaf s r = if ((r !! 0) < 0.5) then (Lit (r !! 1)) else (Var (getPositionInList (r !! 1) s))

getFunction f = getPositionInList f availableFunctions

getMutatedChildren (c1,c2) r = (maybeChangeTU c1 (r !! 0) (r !! 1), maybeChangeTU c2 (r !! 2) (r !! 3))

maybeChangeTU c f1 f2 = if (f1 < mutateChildChance) then getPositionInList f2 availableTU else c

getMutatedLeafProperty s r (Var a) = if (r !! 0) < changeVariableChance then (Var (getPositionInList (r !! 1) s)) else (Var a)
getMutatedLeafProperty s r (Lit a) = if (r !! 0) < changeNumberChance then (Lit (a * (lerp changeNumberRange (r !! 1)))) else (Lit a)

getMutatedLeaf s l r = if ((r !! 0) < mutateLeafChance) then getLeaf s (tail r) else getMutatedLeafProperty s (tail r) l

getMutatedFunction f r = if ((r !! 0) < mutateFuncChance) then getFunction (r !! 1) else f

randomNode s r =
  let newChildren = (getPositionInList (r !! 0) availableTU, getPositionInList (r !! 1) availableTU)
      newLeaf = getLeaf s $ Data.List.drop 4 r
      newFunc = getFunction (r !! 3) 
  in  TranslationUnit newChildren newLeaf newFunc

-- mutates a node with the help of the random seed g
mutateTU s g n =
  let (childR, next) = System.Random.split g
      (leafR, funcR) = System.Random.split next
      newChildren = getMutatedChildren (childrenNodes n) $ randD childR
      newLeaf = getMutatedLeaf s (singleNode n) $ randD leafR
      newFunc = getMutatedFunction (function n) $ randD funcR
  in TranslationUnit newChildren newLeaf newFunc

-- gets the returned value of an alternative and an environment 
calcAltValue :: Env -> Alternative -> Double
calcAltValue e a = (calculate (altToTree a) e)

-- lower is better, sum of square difference
calculateTreeFitness :: [Solution] -> Alternative -> Double
calculateTreeFitness s a = sum $ Data.List.map (\b -> (diff a b)**2) s
  where diff s sol = (value sol) - calcAltValue (environment sol) a

-- randomly culls options with a linearly increasing likelohood of culling based on position in list (assumes the list is sorted)
cullAlternatives :: [Double] -> Double -> [Alternative] -> [Alternative]
cullAlternatives ran r al = Data.List.map (\(_,a,_) -> a) $ Data.List.filter (\(i,a,rn) -> (r * 2.0 * (fromIntegral i)) < ((fromIntegral $ length al) * rn)) zipped
  where zipped = zip3 [0..] al ran

newAlternatives :: [String] -> StdGen -> [Alternative] -> [Alternative]
newAlternatives s g a = Data.List.map (getNewAlternativeFrom a s) $ randomGenerators g  

combineAlternatives :: Alternative -> Alternative -> [Double] -> Alternative
combineAlternatives a1 a2 rands = Alternative (maybeChangeTU (startIndex a1) (rands !! 0) (rands !! 1)) $ fromList $ Data.List.map (\(r, c) -> (c, if (r < 0.5) then ((translationUnits a1) ! c) else ((translationUnits a2) ! c))) $ zip (Data.List.drop 2 rands) availableTU

getNewAlternativeFrom :: [Alternative] -> [String] -> StdGen -> Alternative
getNewAlternativeFrom a s g =
  let randGs = randomGenerators g
      rands = randD g
      cpAlt d = getPositionInList d a
      combinedAlternative = combineAlternatives (cpAlt (rands !! 0)) (cpAlt (rands !! 1)) (Data.List.drop 2 rands)
  in Alternative (startIndex combinedAlternative) $ Data.Map.map (mutateTU s (randGs !! 1)) (translationUnits combinedAlternative)

sortAlternatives sols = sortBy (comparing (calculateTreeFitness sols))

nextGenG :: ([Solution], [Alternative], StdGen) -> ([Solution], [Alternative], StdGen)
nextGenG (sols, al,g) = (sols, getNextGeneration sols g1 al, g2 )
  where (g1, g2) = (System.Random.split g)

gatherData :: [Alternative] -> [Solution] -> StdGen -> [(Double, Tree)] -- (Best fitness, Best Tree)
gatherData al sol g =
  let generations = Data.List.map (\(_,a,_) -> a) $ (iterate nextGenG (sol, al, g)) 
      nonEmptyGens = Data.List.filter (\a -> length a > 0) generations
      fitnessValues = Data.List.map (\a -> ((calculateTreeFitness sol) (a !! 0), altToTree (a !! 0))) nonEmptyGens
  in  fitnessValues

getNextGeneration :: [Solution] -> StdGen -> [Alternative] -> [Alternative]
getNextGeneration sols g alts =
  let [g1,g2,g3,g4] = Data.List.take 4 $ randomGenerators g
      availableStrings = getVariables (sols !! 0)
      sorted = sortAlternatives sols alts-- sort the alternatives for culling
      filterNotNan = Data.List.filter ((not . isNaN) . (calculateTreeFitness sols)) -- remove equations that return invalid values
      nextGen = filterNotNan $ cullAlternatives (randD g1) cullRatio sorted -- removed culled alternatives
      newGen = if (length nextGen) == 0 then (getRandomAlternatives numAlternatives availableStrings g2) else (Data.List.take (numAlternatives - (length nextGen)) $ newAlternatives availableStrings g2 nextGen)
  in  nextGen ++ newGen 

getRandomAlternative s g = Alternative (getPositionInList ((randD g) !! 0) availableTU) $ fromList $ zip availableTU (Data.List.map ((randomNode s). (randD)) (randomGenerators g))

--getRandomAlternatives :: Integer -> [String] -> StdGen -> [Alternative]
getRandomAlternatives n s g = Data.List.take n $ (Data.List.map (getRandomAlternative s) $ randomGenerators g)

outputNicely (d,t) = "\n--------------------------\nBest current solution:\nDistance: " ++ (show d) ++ "\nTree: " ++ (show t)

main = do
  g <- newStdGen
  let rands = randomGenerators g
  solutions <- getSolutions 
  let alternatives = getRandomAlternatives numAlternatives (getVariables (solutions !! 0)) (rands !! 0)
  let res = gatherData alternatives solutions (rands !! 1) 
  mapM_ putStrLn $ [outputNicely x | x <- res]
  return res
