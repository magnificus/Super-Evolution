module EvolutionTypes (module EvolutionTypes) where

import Data.Map
import Data.List
import Data.Ord

data Tree = EmptyTree | Node Func Tree Tree | Leaf Leaf
data Func = Add | Sub | Mul | Pow | Log | Div
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

instance Show Tree where
    show EmptyTree = ""
    show (Leaf (Lit x)) = show x
    show (Leaf (Var x)) = show x
    show (Node f t1 t2) = "(" ++ (show t1) ++ " " ++ (show f) ++ " " ++ (show t2) ++ ")"

instance Show Leaf where
   show (Lit a) = show a
   show (Var a) = show a

-- this is our evolutionary organism, just a map of Integers and translationUnits, the origin is the one that corresponds to '1'
type Alternative = Map Integer TranslationUnit

alternativeTop a = a ! 1 
