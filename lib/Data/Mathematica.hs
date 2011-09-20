{-# LANGUAGE DeriveDataTypeable, BangPatterns #-}

module Data.Mathematica where

import Data.Typeable
import Data.Data

{-
data Atom = MSymbol String 
          | MNumber Double 
          | MString String 
            deriving (Show, Eq, Typeable, Data)
-}

data MExpression = MSymbol  !String 
                 | MInteger !Integer
                 | MReal    !Double
                 | MString  !String
                 | MExp { exp_head :: MExpression
                        , exp_elements :: [MExpression] } 
                   deriving (Show, Eq, Typeable, Data)