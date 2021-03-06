module Main where

import Eval (eval)
import Read (runRepl, runOne)
import Control.Monad (liftM)
import System.Environment (getArgs)

main :: IO ()
main =
  do args <- getArgs
     case length args of
       0 -> runRepl
       1 -> runOne $ args !! 0
       otherwise -> putStrLn "Program takes only 0 or 1 arguments"