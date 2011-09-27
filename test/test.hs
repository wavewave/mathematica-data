{-# LANGUAGE OverloadedStrings #-}

import Data.Attoparsec 
import Data.Mathematica
import Data.Mathematica.Parser 
import qualified Data.ByteString.Char8 as B

import Data.Enumerator
import qualified Data.Enumerator.List as EL
import Data.Enumerator.Binary
import Data.Attoparsec.Enumerator

import System.Environment 
import System.IO

a = B.pack "test sls { a,3,ss, {3,2,3}  } "

b = B.pack "-33920. slkflds "
c = B.pack "" 
d = B.pack "{{a, {a,c,d}}}"
e = B.pack "{{3.48393929, {3.23, -33.06}}}"
f = B.pack "3.23,-33.06"

main = do 
  args <- getArgs
  let fn = args !! 0 
  putStrLn $ "parse " ++ fn

  bstr <- B.readFile fn

  let iter = iterParser manylines 
{-  let iter = do 
        r <-  iterParser line -}
        
      
  h <- openFile fn ReadMode
  r <- run_ $ enumHandle 4096 h $$ iter 

  -- print r
  putStrLn $ show (Prelude.length r)
  hClose h 
{-
  let r = parse manylines bstr

  print r 

  case r of 
    Partial c -> do 
      let Done _ lst = (c B.empty)
      putStrLn $ show (length lst)
-}