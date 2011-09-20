module Data.Mathematica.Parser where

import Data.Mathematica 

import Prelude hiding (takeWhile)
import Control.Applicative hiding (many)

-- import qualified Data.Attoparsec as P
-- import Data.Attoparsec.Combinator 
import Data.Attoparsec.Char8 

import qualified Data.ByteString.Char8 as BC

alphanumeric :: Char -> Bool
alphanumeric = (||) <$> isAlpha_ascii <*> isDigit  

mstring :: Parser MExpression 
mstring = do 
  c <- satisfy (isAlpha_ascii)
  cs <- BC.unpack <$> takeWhile alphanumeric
  return $ MString (c:cs) 


item :: Parser MExpression
item = mstring
       <|> mnumber
       <|> listitem 

{-
mnumber :: Parser MExpression 
mnumber = do n <- number 
             case n of 
               I x -> return (MInteger (show x))
               D x -> return (MReal (show x))
-}

isNumSym :: Char -> Bool 
isNumSym c = c `elem` "0123456789.Ee+-" 

mnumber :: Parser MExpression 
mnumber = do sign <- (try (char '+' <|> char '-')
                    <|> return '+' ) 
             pre <- (sign : ) . BC.unpack <$> takeWhile1 isDigit
             (try (MReal . (pre ++) 
                   <$> ((:) <$> satisfy (inClass ".Ee+-")
                            <*> (BC.unpack <$> takeWhile isNumSym)))
              <|> return (MInteger pre))



listitem :: Parser MExpression 
listitem = do 
  char '{'
  syms <- 
    (try (do sym1 <- skipBlank *> item <* skipBlank
             symr <- many ( char ',' *> skipBlank *> item <* skipBlank)
             return (sym1:symr))
     <|> skipBlank *> return [])
  char '}'
  return $ MExp (MSymbol "List") syms
       

skipBlank :: Parser ()
skipBlank = takeWhile (inClass " \t") >> return ()

line :: Parser [MExpression]
line = many ( skipBlank *> item  <* skipBlank  ) 
  

manylines :: Parser [[MExpression]]
manylines = many (line <* endOfLine) 

