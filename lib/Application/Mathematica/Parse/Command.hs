module Application.Mathematica.Parse.Command where

import Application.Mathematica.Parse.ProgType
import Application.Mathematica.Parse.Job

commandLineProcess :: Mathematica_data -> IO ()
commandLineProcess Test = do 
  putStrLn "test called"
  startJob
