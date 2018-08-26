module SuperEvolution where

data Tree = EmptyTree | Node Func Tree Tree | Leaf Leaf
data Func = Add | Sub
data Leaf = Lit Double | Var Char

calculate :: Tree -> Double
calculate EmptyTree = 0
calculate (Node f t1 t2 ) = calcFunc f (calculate t1) (calculate t2)
calculate (Leaf l) = calcLeaf l

calcLeaf :: Leaf -> Double
calcLeaf (Lit a) = a
calcLeaf (Var a) = 1

calcFunc :: Func -> Double -> Double -> Double
calcFunc Add t1 t2 = t1 + t2
calcFunc Sub t1 t2 = t1 - t2

instance Show Func where
   show Add = "+"
   show Sub = "-"

instance Show Tree where
    show EmptyTree = ""
    show (Leaf (Lit x)) = show x
    show (Leaf (Var x)) = show x
    show (Node f t1 t2) = (show t1) ++ " " ++ (show f) ++ " " ++ (show t2)
