module EvolutionParser (module EvolutionParser) where

import Data.Char
import Control.Monad
import Data.Map
import Data.List
import Text.ParserCombinators.Parsec
import Data.Either
import Data.Maybe
import EvolutionTypes

type Input = [[(String, String)]] -- list of list of string pairs, each list correspond to one solution, expects definitions of variables and "res"

ident :: Parser String
ident = do c <- letter <|> char '_'
           cs <- many (letter <|> digit <|> char '_')
           return (c:cs)
      <?> "identifier"

eos :: Parser ()
eos = do oneOf " ,"
         return ()
    <?> "end of statement"

eol :: Parser ()
eol = do oneOf "\r\n"
         return ()
    <?> "end of line"

emptyChars = oneOf " \t"

item :: Parser (String, String)

item = do skipMany emptyChars 
          key <- ident
          skipMany emptyChars
          char '='
          skipMany emptyChars
          value <- manyTill anyChar (lookAhead $ eol <|> eos <|> eof)
          skipMany emptyChars
          skipMany (char ',')
          return (key, value)


line :: Parser [(String, String)]
line = manyTill item (eol <|> lookAhead eof)

file :: Parser [[(String, String)]]
file = manyTill line eof

getSolutions :: IO [Solution]
getSolutions = do
    inputEithers <- readInput "input.txt"
    solutions <- case inputEithers of
        Left err -> return [(Solution (fromList []) 0)]
        Right input -> return $ Data.List.map toSolution input
    return solutions

readInput :: String -> IO (Either ParseError Input)
readInput = parseFromFile file 

-- takes the string inputs and transforms them into the solution data type for use in program
toSolution i = Solution (Data.Map.delete "res" mapped) (mapped ! "res")
    where mapped = fromList $ Data.List.map (\(a,b) -> (a, read b :: Double)) i
