module Data.Mathematica.Command where

import Data.Mathematica.Type
import Data.Mathematica.Job

commandLineProcess :: Mathematica_data -> IO ()
commandLineProcess Test = do 
  putStrLn "test called"
  startJob
