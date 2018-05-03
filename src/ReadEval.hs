module ReadEval where

import Read
import Eval
import Error (LispError)
import Prims
import Lisp
import Control.Monad (liftM)
import Data.Either
import Env
import System.IO hiding (try)

-- readEval :: String -> IO ()
-- readEval = runOne
evalStringPs
  :: [Primitive] -> Env -> String -> IO String
evalStringPs ps env expr =
  runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= evalPs ps env

evalStringNEnv
  :: [Primitive] -> String -> IO String
evalStringNEnv ps expr =
  do nenv <- nullEnv
     evalStringPs ps nenv expr

-- readEval :: String -> Either LispError String
-- readEval s = liftM show $ readExpr s >>= (eval nullEnv)
-- readEvalPs
--   :: [Primitive] -> String -> Either LispError String
-- readEvalPs prims s = liftM show $ readExpr s >>= (evalPs prims)
-- readEPStr :: [Primitive] -> String -> String
-- readEPStr prims = fromRight "error" . readEvalPs prims
blah = id