module EvolutionParser (module EvolutionParser) where

import Data.Char
import Control.Monad
import Data.Map
import Data.List
import Text.ParserCombinators.Parsec
import Data.Either
import Data.Maybe

type Config = [Data.Map.Map String String]


ident :: Parser String

ident = do c <- letter <|> char '_'
           cs <- many (letter <|> digit <|> char '_')
           return (c:cs)
      <?> "identifier"


eol :: Parser ()
eol = do oneOf "\n\r"
         return ()
    <?> "end of line"

item :: Parser (String, String)

item = do key <- ident
          skipMany space
          char '='
          skipMany space
          value <- manyTill anyChar (try eol <|> eof)
          return (key, rstrip value)
    where rstrip = reverse . dropWhile isSpace . reverse


line :: Parser (String, String)
line = do skipMany space
          try (item >>= return)
