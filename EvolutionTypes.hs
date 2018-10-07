module EvolutionTypes (module EvolutionTypes) where

import Data.Map
import Data.List
import Data.Ord

data Tree = EmptyTree | Node Func Tree Tree | Leaf Leaf
data Func = Add | Sub | Mul | Pow | Log | Div | LeftOnly | RightOnly
data Leaf = Lit Double | Var Char

type Env = Map Char Double -- An environment is defined as a map of id:s (variables) and their corresponding values
data Solution = Solution {environment :: Env, value :: Double} deriving (Show)-- this type corresponds to an environment together with the ideal ouput result

-- A Translation unit contains the next step (two chars), the single node expression, and the binary node expression
data TranslationUnit = TranslationUnit {childrenNodes :: (Integer, Integer), singleNode :: Leaf, function :: Func } deriving (Show)

instance Show Func where
   show Add = "+"
   show Sub = "-"
   show Mul = "*"
   show Div = "/"
   show Pow = "^"
   show Log = "log"
   show LeftOnly = "LeftOnly"
   show RightOnly = "RightOnly"

instance Show Tree where
    show EmptyTree = ""
    show (Leaf (Lit x)) = show x
    show (Leaf (Var x)) = show x
    show (Node Add t1 t2) = (show t1) ++ " + " ++ (show t2)
    show (Node Sub t1 t2) = (show t1) ++ " - " ++ (show t2)
    show (Node Mul t1 t2) = "(" ++ (show t1) ++ ") * (" ++ (show t2) ++ ")"
    show (Node Div t1 t2) = "(" ++ (show t1) ++ ") / (" ++ (show t2) ++ ")"
    show (Node Pow t1 t2) = "(" ++ (show t1) ++ ") ^ (" ++ (show t2) ++ ")"
    show (Node Log t1 t2) = "logAbs(" ++ (show t1) ++ ")"
    show (Node LeftOnly t1 t2) = show t1
    show (Node RightOnly t1 t2) = show t2

instance Show Leaf where
   show (Lit a) = show a
   show (Var a) = show a

-- this is our evolutionary organism, just a map of Integers and translationUnits, the origin is the one that corresponds to '1'
type Alternative = Map Integer TranslationUnit

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
calcFunc Log t1 t2 = log (abs t1) --(ignores t2)
calcFunc LeftOnly t1 t2 = t1 --(ignores t2)
calcFunc RightOnly t1 t2 = t2 --(ignores t2)

alternativeTop a = a ! 1 

toTree :: TranslationUnit -> Alternative -> Integer -> Tree
toTree t a n
   | n > 1 = Node (function t) (toTree ((!) a . fst $ childrenNodes t) a (n-1)) (toTree  ((!) a . snd $ childrenNodes t) a (n-1))
   | otherwise =  Leaf (singleNode t)
