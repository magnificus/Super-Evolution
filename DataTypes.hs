module DataTypes where

import Control.Applicative
import System.Random
import Data.Map (Map)
import qualified Data.Map as M

type Env = Map Char Double


-- A Translation unit contains the next step (two chars), the single node expression, and the binary node expression
data TranslationUnit a b c = TranslationUnit {childrenNodes :: a, singleNode :: b, binaryNode :: c } deriving (Show)


--translationUnits :: M.Map Char TranslationUnit
translationUnits = M.fromList [('f', TranslationUnit ('f','g') (Lit 1) (Add))]

variableValues = M.fromList [('x', 1.0), ('y', 0.0)]

data Expr
    = Lit Double        -- Literal numbers
    | Var Char          -- Variables have single letter names
    | Add Expr Expr     -- We can add things together
    | Sub Expr Expr     -- And subtract them
    | Mul Expr Expr     -- Why not multiply, too?
    | Div Expr Expr     -- And divide
    deriving (Eq)

instance Show Expr where
    showsPrec n (Lit x)   = showParen (n > 10) $ showsPrec 11 x
    showsPrec n (Var x)   = showParen (n > 10) $ showChar x
    showsPrec n (Add x y) = showParen (n >  6) $ showsPrec 7 x . showString " + " . showsPrec 7 y
    showsPrec n (Sub x y) = showParen (n >  6) $ showsPrec 7 x . showString " - " . showsPrec 7 y
    showsPrec n (Mul x y) = showParen (n >  7) $ showsPrec 8 x . showString " * " . showsPrec 8 y
    showsPrec n (Div x y) = showParen (n >  7) $ showsPrec 8 x . showString " / " . showsPrec 8 y

instance Num Expr where
    fromInteger = Lit . fromInteger
    (+) = Add
    (-) = Sub
    (*) = Mul
    abs = undefined
    signum = undefined

instance Fractional Expr where
    (/) = Div
    fromRational = Lit . fromRational



binOp :: (Double -> Double -> Double) -> Expr -> Expr -> Env -> Maybe Double
binOp op x y vars = op <$> evalExpr x vars <*> evalExpr y vars

evalExpr :: Expr -> Env -> Maybe Double
evalExpr (Lit x)   = const $ Just x
evalExpr (Var x)   = M.lookup x
evalExpr (Add x y) = binOp (+) x y
evalExpr (Sub x y) = binOp (-) x y
evalExpr (Mul x y) = binOp (*) x y
evalExpr (Div x y) = binOp (/) x y

randomLit, randomVar :: IO Expr
randomLit = Lit <$> randomRIO (-100, 100)
randomVar = Var <$> randomRIO ('x',  'z')

--
--
--

--generateExpr :: TranslationUnit -> Int -> Expr
generateExpr t 1 = singleNode t

generateExpr t n = generateExpr t (n-1)
    
--generateExpr :: Int -> TranslationUnit -> IO Expr
--generateExpr t 1 = do
--    isLit <- randomIO
--    if isLit
--        then randomLit
--        else randomVar
-- Otherwise, generate a tree using helper
--generateExpr t n = randomRIO (0, 100) >>= helper
--    where
--        helper :: Int -> IO Expr
--        helper prob
--            -- 20% chance that it's a literal
--            | prob < 20  = randomLit
--            -- 10% chance that it's a variable
--            | prob < 30  = randomVar
--            -- 15% chance of Add
--            | prob < 45  = (+) <$> generateExpr (n - 1) <*> generateExpr (n - 1)
--            -- 15% chance of Sub
--            | prob < 60  = (-) <$> generateExpr (n - 1) <*> generateExpr (n - 1)
--            -- 15% chance of Mul
--            | prob < 75  = (*) <$> generateExpr (n - 1) <*> generateExpr (n - 1)
--            -- 15% chance of Div
--            | prob < 90  = (/) <$> generateExpr (n - 1) <*> generateExpr (n - 1)
--            -- 10% chance that we generate a possibly taller tree
--            | otherwise = generateExpr (n + 1)
