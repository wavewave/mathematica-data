module Data.Mathematica.Parser where

import Data.Mathematica 

import Control.Applicative hiding (many)

-- import qualified Data.Attoparsec as P
-- import Data.Attoparsec.Combinator 
import Data.Attoparsec.Char8 

import qualified Data.ByteString.Char8 as BC

alphanumeric :: Char -> Bool
alphanumeric = (||) <$> isAlpha_ascii <*> isDigit  

item =   mstring
     <|> listitem 

number :: Parser String 
number = do sign <- (try (char '+' <|> char '-')
                    <|> return '+' ) 
            digits <- BC.unpack <$> takeWhile1 isDigit
            return (sign : digits)

listitem :: Parser MExpression 
listitem = do 
  char '{'
  syms <- 
    (try (do sym1 <- skipBlank *> mstring <* skipBlank
             symr <- many ( char ',' *> skipBlank *> item <* skipBlank)
             return (sym1:symr))
     <|> skipBlank *> return [])
  char '}'
  return $ MExp (MSymbol "List") syms
       
mstring :: Parser MExpression 
mstring = do 
  x <- BC.unpack <$> takeWhile1 alphanumeric
  return $ MString x 

skipBlank :: Parser ()
skipBlank = many (char ' ') >> return ()

line :: Parser [MExpression]
line = do 
  ws <- many1 ( skipBlank *> item  <* skipBlank  ) 
  endOfLine 
  return ws 


{-
oneline  = do 
  many1 (item)  


item = list <|> 
-}