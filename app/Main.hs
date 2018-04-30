module Main where

import Parse
import Eval
import Error
import Read
import Control.Monad
import System.Environment

main :: IO ()
main =
  do args <- getArgs
     evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
     putStrLn $ extractValue $ trapError evaled