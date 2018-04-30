module ReadEval where

import Read (readExpr)
import Eval (eval)
import Error (LispError)
import Control.Monad (liftM)

readEval :: String -> Either LispError String
readEval s = liftM show $ readExpr s >>= eval