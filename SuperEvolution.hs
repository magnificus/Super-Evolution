module Main where

import System.Random
import Data.Map
import Data.List
import Data.Ord
import Control.Monad
import Data.Maybe

import EvolutionTypes
import EvolutionConfig

calculate :: Tree -> Env -> Double
calculate EmptyTree _ = 0
calculate (Node f t1 t2 ) e = calcFunc f (calculate t1 e) (calculate t2 e)
calculate (Leaf l) e = calcLeaf l e

calcLeaf :: Leaf -> Env -> Double
calcLeaf (Lit a) _ = a
calcLeaf (Var a) e = e ! a

calcFunc :: Func -> Double -> Double -> Double
calcFunc Add t1 t2 = t1 + t2
calcFunc Sub t1 t2 = t1 - t2
calcFunc Mul t1 t2 = t1 * t2
calcFunc Div t1 t2 = t1 / t2
calcFunc Pow t1 t2 = t1 ** t2
calcFunc Log t1 t2 = logBase t1 t2

toTree :: TranslationUnit -> Alternative -> Integer -> Tree
toTree t a n
   | n > 1 = Node (function t) (toTree ((!) a . fst $ childrenNodes t) a (n-1)) (toTree  ((!) a . snd $ childrenNodes t) a (n-1))
   | otherwise =  Leaf (singleNode t)

altToTree :: Alternative -> Tree
altToTree a = toTree (alternativeTop a) a treeDepth

getPositionInList f l = l !! (floor $ f* (fromIntegral $ length l))

getTU :: Double -> [Integer] -> Integer
getTU f s = getPositionInList f $ s

getLeaf :: [Double] -> Leaf
getLeaf r = if ((r !! 0) < 0.5) then (Lit (r !! 1)) else (Var (getPositionInList (r !! 1) charsSol))

getFunction f = getPositionInList f availableFunctions

getMutatedChildren (c1,c2) r = (maybeChange c1 (r !! 0) (r !! 1), maybeChange c2 (r !! 2) (r !! 3))
   where maybeChange c f1 f2 = if (f1 < mutateChildChance) then getTU f2 availableTU else c

getMutatedLeafProperty r (Var a) = if (r !! 0) < changeVariableChance then (Var (getPositionInList (r !! 1) charsSol)) else (Var a)
getMutatedLeafProperty r (Lit a) = if (r !! 0) < changeNumberChance then (Lit (a * (randInRange changeNumberRange (r !! 1)))) else (Lit a)

getMutatedLeaf l r = if ((r !! 0) < mutateLeafChance) then getLeaf (tail r) else getMutatedLeafProperty (tail r) l

getMutatedFunction f r = if ((r !! 0) < mutateFuncChance) then getFunction (r !! 1) else f



randomNode r =
  let newChildren = (getTU (r !! 0) availableTU, getTU (r !! 1) availableTU)
      newLeaf = getLeaf $ Data.List.drop 4 r
      newFunc = getFunction (r !! 3) 
  in  TranslationUnit newChildren newLeaf newFunc

mutateNode g n =
  let (childR, next) = System.Random.split g
      (leafR, funcR) = System.Random.split next
      newChildren = getMutatedChildren (childrenNodes n) $ randD childR
      newLeaf = getMutatedLeaf (singleNode n) $ randD leafR
      newFunc = getMutatedFunction (function n) $ randD funcR
  in TranslationUnit newChildren newLeaf newFunc
 
--randDouble = randomRIO (0.0::Double,0.9999::Double)
randD :: StdGen -> [Double] -- returns an infinite list of random doubles
randD = randomRs (0.0::Double, 0.9999::Double)

calcAltValue :: Env -> Alternative -> Double
calcAltValue e a = (calculate (altToTree a) e)

-- lower is better
calculateTreeFitness :: [Solution] -> Alternative -> Double
calculateTreeFitness s a = Data.List.foldl' (\a b -> a + (diff s b)**2) 0 s
  where diff s sol = (value sol) - calcAltValue (environment sol) a


createSolutions f = Prelude.map (\a -> Solution (fromList [('x', a)]) $ f a) [0.0..100.0] 

defaultSolutions = createSolutions (\x -> x*x + x + 4.2 ) -- x ^ 2 + x + 4.2 is the solution

cullAlternatives :: [Double] -> Double -> [Alternative] -> [Alternative]
cullAlternatives ran r al = Data.List.map (\(_,a,_) -> a) $ Data.List.filter (\(i,a,rn) -> (r * 2.0 * (fromIntegral i)) <= ((fromIntegral $ length al) * rn)) zipped
  where zipped = zip3 [0..] al ran

newAlternatives n g a = Data.List.take n $ Data.List.map (getNewAlternativeFrom a) $ randomGenerators g  

combineAlternatives :: Alternative -> Alternative -> [Double] -> Alternative
combineAlternatives a1 a2 rands = fromList $ Data.List.map (\(r, c) -> (c, if (r < 0.5) then (a1 ! c) else (a2 ! c))) $ zip rands availableTU

getNewAlternativeFrom :: [Alternative] -> StdGen -> Alternative
getNewAlternativeFrom a g =
  let randGs = randomGenerators g
      rands = randD g
      cpAlt d = getPositionInList d a
      combinedAlternative = combineAlternatives (cpAlt (rands !! 0)) (cpAlt (rands !! 1)) (Data.List.drop 2 rands)
  in Data.Map.map (mutateNode (randGs !! 1)) combinedAlternative

--randomList = randD <$> newStdGen

randInRange (a, b) f = (b-a)*f + a

sortAlternatives = sortBy (comparing (calculateTreeFitness defaultSolutions))

randomGenerators g = Data.List.unfoldr (\g1 -> Just $ System.Random.split g1) g

nextGenG :: ([Alternative], StdGen) -> ([Alternative], StdGen)
nextGenG (al,g) = (getNextGeneration g1 al, g2 )
  where (g1, g2) = (System.Random.split g)

gatherData :: [Alternative] -> [Solution] -> StdGen -> [(Double,Double,Tree)] -- (Best, Average, Best Tree)
gatherData al sol g =
  let generations = Data.List.map fst $ (iterate nextGenG (al, g)) 
      fitnessValues = Data.List.map (\a -> ((calculateTreeFitness sol) (a !! 0), (sum (Data.List.map (calculateTreeFitness sol) a)) / (Data.List.genericLength a), altToTree (a !! 0))) generations
  in fitnessValues

getNextGeneration :: StdGen -> [Alternative] -> [Alternative]
getNextGeneration g alts =
  let sorted = sortAlternatives alts -- sort the alternatives for culling
      (g1,g2) = System.Random.split g
      nextGen = Data.List.filter ((not . isNaN) . (calculateTreeFitness defaultSolutions)) $ cullAlternatives (randD g1) cullRatio sorted -- removed culled alternatives
      newGen = newAlternatives (numAlternatives - (length nextGen)) g2 nextGen
  in  nextGen ++ newGen 

getRandomAlternative g = fromList $ zip availableTU (Data.List.map (randomNode . randD) (randomGenerators g))

getRandomAlternatives n g = Data.List.take n $ (Data.List.map getRandomAlternative $ randomGenerators g)

main = do
  g <- newStdGen
  let (g1,g2) = System.Random.split g
  let alternatives = getRandomAlternatives numAlternatives g1
  let res = gatherData alternatives defaultSolutions g2
  mapM_ putStrLn $ Data.List.map show res
  return res
