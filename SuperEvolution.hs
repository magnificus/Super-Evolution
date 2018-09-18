module SuperEvolution where

import System.Random
import Data.Map
import Data.List
import Data.Ord
import Control.Monad

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

defaultTU1 = TranslationUnit ('a', 'a') (Lit 1) Add

--defaultSolution :: Solution
--defaultSolution = Solution defaultEnv (calculate (toTree defaultTU1 defaultAlternative treeDepth) defaultEnv)

defaultAlternative = fromList [('a', defaultTU1), ('b', defaultTU1), ('c', defaultTU1)]

toTree :: TranslationUnit -> Alternative -> Integer -> Tree
toTree t a n
   | n > 1 = Node (function t) (toTree ((!) a . fst $ childrenNodes t) a (n-1)) (toTree  ((!) a . snd $ childrenNodes t) a (n-1))
   | otherwise =  Leaf (singleNode t)

getPositionInList f l = l !! (floor $ f* (fromIntegral $ length l))

getTUChar :: Double -> String -> Char
getTUChar f s = getPositionInList f $ s

getLeaf :: Double -> Double -> Leaf
getLeaf f1 f2 = if (f1 < 0.5) then (Lit f2) else (Var (getTUChar f2 charsSol))

getFunction f = getPositionInList f availableFunctions

getMutatedChildren (c1,c2) f1 f2 f3 f4 = (maybeChange c1 f1 f2, maybeChange c2 f3 f4)
   where maybeChange c f1 f2 = if (f1 < mutateChildChance) then getTUChar f2 charsTU else c

getMutatedLeaf l f1 f2 f3 = if (f1 < mutateLeafChance) then getLeaf f2 f3 else l

getMutatedFunction f f1 f2 = if (f1 < mutateFuncChance) then getFunction f2 else f

getRandomNode = do
  randomNode <$> randDouble <*> randDouble <*> randDouble <*> randDouble <*> randDouble

randomNode f1 f2 f3 f4 f5 = do
  let newChildren = (getTUChar f1 charsTU, getTUChar f2 charsTU)
  let newLeaf = getLeaf f3 f4
  let newFunc = getFunction f5
  TranslationUnit newChildren newLeaf newFunc

mutateNode n = do
  let newChildren = getMutatedChildren (childrenNodes n) <$> randDouble <*> randDouble <*> randDouble <*> randDouble
  let newLeaf = getMutatedLeaf (singleNode n) <$> randDouble <*> randDouble <*> randDouble
  let newFunc = getMutatedFunction (function n) <$> randDouble <*> randDouble
  TranslationUnit <$> newChildren <*> newLeaf <*> newFunc
 
randDouble = randomRIO (0.0::Double,0.9999::Double)

calcTreeValue :: Env -> Alternative -> TranslationUnit -> Double
calcTreeValue e a t = (calculate (toTree t a treeDepth) e)

-- lower is better, our top Node is the one c 
calculateTreeFitness :: [Solution] -> Alternative -> Double
calculateTreeFitness s a = Prelude.foldl (\a b -> a + (diff s b)**2) 0 s
  where diff s sol = (value sol) - calcTreeValue (environment sol) a (alternativeTop a)


defaultSolutions = Prelude.map (\a -> Solution (fromList [('x', a)]) $ a^2) [1.0..100.0] 

exportIO (a, b) = do { b2 <- b; return (a,b2) }

main = do
  let alternative = liftM fromList $ sequence $ Data.List.map exportIO $ zip charsTU (repeat $ getRandomNode) -- gets one random alternative
  alternatives <- sequence $ Data.List.take numAlternatives $ repeat $ alternative -- gets several random alternatives
  let sorted = sortBy (comparing (calculateTreeFitness defaultSolutions)) alternatives
  
  return (sorted !! 0)
  --return sorted
  --return alternatives
  --trees <- sequence $ Prelude.take aumTrees $ repeat getRandomNode -- these are the first generation trees
  --let sorted = sortBy (comparing (calculateTreeFitness defaultSolutions)) trees
  --return sorted
