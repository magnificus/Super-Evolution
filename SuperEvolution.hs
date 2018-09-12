module SuperEvolution where
import Data.Map
import System.Random
type Env = Map Char Double

mutateChildChance = 0.1
mutateLeafChance = 0.1
mutateFuncChance = 0.1

numTrees = 10

defaultMap = fromList [('x', 1.0), ('y', 2.0)]
defaultTree = (Node Sub (Node Add (Leaf (Var 'x')) (Leaf (Lit 2))) (Leaf (Var 'y')))

data Tree = EmptyTree | Node Func Tree Tree | Leaf Leaf
data Func = Add | Sub | Mul
data Leaf = Lit Double | Var Char

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

instance Show Func where
   show Add = "+"
   show Sub = "-"
   show Mul = "*"

instance Show Tree where
    show EmptyTree = ""
    show (Leaf (Lit x)) = show x
    show (Leaf (Var x)) = show x
    show (Node f t1 t2) = (show t1) ++ " " ++ (show f) ++ " " ++ (show t2)

instance Show Leaf where
   show (Lit a) = show a
   show (Var a) = [a]

-- A Translation unit contains the next step (two chars), the single node expression, and the binary node expression
data TranslationUnit = TranslationUnit {childrenNodes :: (Char, Char), singleNode :: Leaf, function :: Func } deriving (Show)

defaultTU1 = TranslationUnit ('f', 'x') (Lit 1) Add
defaultTU2 = TranslationUnit ('x', 'w') (Var 'x') Sub 
defaultTU3 = TranslationUnit ('w', 'f') (Lit 3) Mul 

treeMap = fromList [('f', defaultTU1), ('x', defaultTU2), ('w', defaultTU3)]
availableFunctions = [Add,Sub,Mul]

charToTree = (!) treeMap

toTree :: TranslationUnit -> Integer -> Tree
toTree t n
   | n > 1 = Node (function t)  (toTree (charToTree . fst $ childrenNodes t) (n-1)) (toTree  (charToTree . snd $ childrenNodes t) (n-1))
   | otherwise =  Leaf (singleNode t)

getPositionInList f l = l !! (floor $ f* (fromIntegral $ length l))

getTUChar :: Double -> Char
getTUChar f = getPositionInList f $ keys treeMap

getLeaf :: Double -> Double -> Leaf
getLeaf f1 f2 = if (f1 < 0.5) then (Lit f2) else (Var (getTUChar f2))

getFunction f = getPositionInList f availableFunctions

getMutatedChildren (c1,c2) f1 f2 f3 f4 = (maybeChange c1 f1 f2, maybeChange c2 f3 f4)
   where maybeChange c f1 f2 = if (f1 < mutateChildChance) then getTUChar f2 else c

getMutatedLeaf l f1 f2 f3 = if (f1 < mutateLeafChance) then getLeaf f2 f3 else l

getMutatedFunction f f1 f2 = if (f1 < mutateFuncChance) then getFunction f2 else f

getRandomNode = do
  randomNode <$> randDouble <*> randDouble <*> randDouble <*> randDouble <*> randDouble

randomNode f1 f2 f3 f4 f5 = do
  let newChildren = (getTUChar f1, getTUChar f2)
  let newLeaf = getLeaf f3 f4
  let newFunc = getFunction f5
  TranslationUnit newChildren newLeaf newFunc

mutateNode n = do
  let newChildren = getMutatedChildren (childrenNodes n) <$> randDouble <*> randDouble <*> randDouble <*> randDouble
  let newLeaf = getMutatedLeaf (singleNode n) <$> randDouble <*> randDouble <*> randDouble
  let newFunc = getMutatedFunction (function n) <$> randDouble <*> randDouble
  TranslationUnit <$> newChildren <*> newLeaf <*> newFunc
 
randDouble = randomRIO (0.0::Double,0.9999::Double)

calcTreeValue t e = (calculate (toTree t 4) e)

--calculateTreeFitness t e l = 

main = do
  trees <- sequence $ Prelude.take numTrees $ repeat getRandomNode -- these are the first generation trees
  newNodes <- sequence $ Prelude.map mutateNode trees
  return newNodes 
