{-# LANGUAGE DeriveDataTypeable #-}

module Application.Mathematica.Parse.ProgType where 

import System.Console.CmdArgs

data Mathematica_data = Test 
              deriving (Show,Data,Typeable)

test :: Mathematica_data
test = Test 

mode = modes [test]

