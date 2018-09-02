module SuperEvolution where
import Data.Map

type Env = Map Char Double

--translationMap = 
-- A Translation unit contains the next step (two chars), the single node expression, and the binary node expression

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


data TranslationUnit = TranslationUnit {childrenNodes :: (TranslationUnit, TranslationUnit), singleNode :: Leaf, function :: Func }

defaultTU1 = TranslationUnit (defaultTU2, defaultTU1) (Lit 1) Add
defaultTU2 = TranslationUnit (defaultTU1, defaultTU3) (Var 'x') Sub 
defaultTU3 = TranslationUnit (defaultTU1, defaultTU2) (Lit 3) Mul 



toTree :: TranslationUnit -> Integer -> Tree
toTree t n
   | n > 1 = Node (function t)  (toTree (fst $ childrenNodes t) (n-1)) (toTree (snd $ childrenNodes t) (n-1))
   | otherwise =  Leaf (singleNode t)
