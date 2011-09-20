module Main where

import System.Console.CmdArgs

import Application.Mathematica.Parse.ProgType
import Application.Mathematica.Parse.Command

main :: IO () 
main = do 
  putStrLn "mathematica-data"
  param <- cmdArgs mode

  commandLineProcess param