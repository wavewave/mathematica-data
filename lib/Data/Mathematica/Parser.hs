module Data.Mathematica.Parser where

import Data.Mathematica 

import Prelude hiding (takeWhile)
import Control.Applicative 

-- import qualified Data.Attoparsec as P
-- import Data.Attoparsec.Combinator 
import Data.Attoparsec.Char8 

import qualified Data.ByteString.Char8 as BC

import Debug.Trace

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

readDouble :: String -> Double 
readDouble str 
  | (not.null) str = let rstr = reverse str 
                         x = head rstr
                     in if x == '.'  
                          then read $ reverse ('0' : rstr)
                          else read str 
  | otherwise = error "Error in readDouble otherwise"

isNumSym :: Char -> Bool 
isNumSym c = c `elem` "0123456789.Ee+-" 

mnumber :: Parser MExpression 
mnumber = do sign <- (try (char '+' <|> char '-')
                      <|> return '+' ) 
             pre <- if sign == '+' 
                      then BC.unpack <$> takeWhile1 isDigit
                      else ('-' : ) . BC.unpack <$> takeWhile1 isDigit
             
             (try ( do {
                str <-  (pre ++) <$> ((:) <$> satisfy (inClass ".Ee+-")
                                          <*> (BC.unpack <$> takeWhile isNumSym)) ; 
                {- trace ('D':str) $ -}
                return (MReal (readDouble str)) 
                } ) <|> ({- trace ("I" ++ pre) $ -} return (MInteger (read pre))))
             
             



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

