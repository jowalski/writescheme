module Read where

import Lisp
import Parse
import Error
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad.Except

readExpr :: String -> ThrowsError LispVal
readExpr input =
  case parse parseExpr "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val