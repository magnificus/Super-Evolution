module EvolutionTypes (module EvolutionTypes) where

import Data.Map
import Data.List
import Data.Ord

data Tree = EmptyTree | Node Func Tree Tree | Leaf Leaf
data Func = Add | Sub | Mul
data Leaf = Lit Double | Var Char

type Env = Map Char Double -- An environment is defined as a map of chars (variables) and their corresponding values
data Solution = Solution {environment :: Env, value :: Double} deriving (Show)-- this type corresponds to an environment together with the ideal ouput result

-- A Translation unit contains the next step (two chars), the single node expression, and the binary node expression
data TranslationUnit = TranslationUnit {childrenNodes :: (Char, Char), singleNode :: Leaf, function :: Func } deriving (Show)

instance Show Func where
   show Add = "+"
   show Sub = "-"
   show Mul = "*"

instance Show Tree where
    show EmptyTree = ""
    show (Leaf (Lit x)) = show x
    show (Leaf (Var x)) = show x
    show (Node f t1 t2) = "(" ++ (show t1) ++ " " ++ (show f) ++ " " ++ (show t2) ++ ")"

instance Show Leaf where
   show (Lit a) = show a
   show (Var a) = [a]

-- this is our evolutionary organism, just a map of chars and translationUnits, the origin is the one that constitutes the letter 'a'
type Alternative = Map Char TranslationUnit

alternativeTop a = a ! 'a' 
