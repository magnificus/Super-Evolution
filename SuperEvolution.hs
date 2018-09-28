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

altToTree :: Alternative -> Tree
altToTree a = toTree (alternativeTop a) a treeDepth

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
randD = randomRs (0.0::Double, 0.9999::Double)

calcTreeValue :: Env -> Alternative -> Double
calcTreeValue e a = (calculate (altToTree a) e)

-- lower is better
calculateTreeFitness :: [Solution] -> Alternative -> Double
calculateTreeFitness s a = Prelude.foldl (\a b -> a + (diff s b)**2) 0 s
  where diff s sol = (value sol) - calcTreeValue (environment sol) a


defaultSolutions = Prelude.map (\a -> Solution (fromList [('x', a)]) $ a^2) [1.0..100.0] -- x ^ 2 is the solution


cullAlternatives :: [Double] -> Double -> [Alternative] -> [Alternative]
cullAlternatives ran r al = Data.List.foldr (\(i,a,rn) l -> if ((r * 2 * (fromIntegral i)) < (fromIntegral $ length al) * rn) then a:l else l) [] zipped
  where zipped = zip3 [0..] al ran

newAlternatives n a = sequence $ Data.List.take n (repeat $ getNewAlternativeFrom a)  

getNewAlternativeFrom :: [Alternative] -> IO Alternative
getNewAlternativeFrom a = do getPositionInList <$> randDouble <*> (return a)

exportIO (a, b) = do { b2 <- b; return (a,b2) }

randomList =  randD <$> newStdGen

getNextGeneration :: IO [Alternative] -> IO [Alternative]
getNextGeneration ioAlternatives = do
  alternatives <- ioAlternatives
  let sorted = sortBy (comparing (calculateTreeFitness defaultSolutions)) alternatives -- sort the alternatives for culling
  currRandomList <- randomList
  let nextGen = cullAlternatives currRandomList cullRatio sorted -- removed culled alternatives
  nextGenFull <- (++) nextGen <$> newAlternatives (numAlternatives - (length nextGen)) nextGen  -- added new replacements
  return nextGenFull

main = do
  let alternative = liftM fromList $ sequence $ Data.List.map exportIO $ zip charsTU (repeat $ getRandomNode) -- gets one random alternative
  let alternatives = sequence $ Data.List.take numAlternatives $ repeat $ alternative -- gets several random alternatives
  iterate getNextGeneration alternatives
   --return $ length nextGen
  --return (nextGen !! 0)
--  let topAlt =  sorted !! 0
--  let topTree = toTree (alternativeTop topAlt)  topAlt treeDepth
  --return topAlt
  --return sorted
  --return alternatives
  --trees <- sequence $ Prelude.take aumTrees $ repeat getRandomNode -- these are the first generation trees
  --let sorted = sortBy (comparing (calculateTreeFitness defaultSolutions)) trees
  --return sorted
