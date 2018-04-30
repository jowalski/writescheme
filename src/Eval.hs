module Eval where

import Lisp
import Error
import Prims
import Control.Monad.Except

eval :: LispVal -> ThrowsError LispVal
eval = evalPs []

-- (readExpr "(symbol->string 'abc)") >>= evalPs symbolPrimitives
evalPs
  :: [Primitive] -> LispVal -> ThrowsError LispVal
evalPs _ val@(String _) = return val
evalPs _ val@(Number _) = return val
evalPs _ val@(Bool _) = return val
evalPs _ (List [Atom "quote",val]) = return val
evalPs ps (List [Atom "if",pred,conseq,alt]) =
  do result <- (evalPs ps) pred
     case result of
       Bool False -> (evalPs ps) pred
       otherwise -> (evalPs ps) conseq
evalPs ps (List (Atom func:args)) =
  mapM (evalPs ps) args >>= (applyPrims ps) func
evalPs _ badForm =
  throwError $ BadSpecialForm "Unrecognized special form" badForm

apply
  :: String -> [LispVal] -> ThrowsError LispVal
apply = applyPrims []

applyPrims
  :: [Primitive] -> String -> [LispVal] -> ThrowsError LispVal
applyPrims ps f args =
  maybe (throwError $ NotFunction "Unrecognized primitive function args" f) ($ args) $
  lookup f (ps ++ primitives)