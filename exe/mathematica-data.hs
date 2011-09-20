module Main where

import System.Console.CmdArgs

import Data.Mathematica.Type
import Data.Mathematica.Command

main :: IO () 
main = do 
  putStrLn "mathematica-data"
  param <- cmdArgs mode

  commandLineProcess param