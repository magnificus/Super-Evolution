module SuperEvolution where
import Data.Map

type Env = Map Char Double


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

charToTree = (!) treeMap

toTree :: TranslationUnit -> Integer -> Tree
toTree t n
   | n > 1 = Node (function t)  (toTree (charToTree . fst $ childrenNodes t) (n-1)) (toTree  (charToTree . snd $ childrenNodes t) (n-1))
   | otherwise =  Leaf (singleNode t)



getTUChar = (!!) (keys treeMap)
