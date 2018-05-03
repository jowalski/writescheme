module Eval where

import Lisp
import Error
import Prims
import Env
import Control.Monad.Except

eval :: Env -> LispVal -> IOThrowsError LispVal
eval = evalPs []

-- (readExpr "(symbol->string 'abc)") >>= evalPs symbolPrimitives
evalPs
  :: [Primitive] -> Env -> LispVal -> IOThrowsError LispVal
evalPs _ env val@(String _) = return val
evalPs _ env val@(Number _) = return val
evalPs _ env val@(Bool _) = return val
evalPs _ env (Atom id) = getVar env id
evalPs _ env (List [Atom "quote",val]) = return val
evalPs ps env (List [Atom "if",pred,conseq,alt]) =
  do result <- (evalPs ps) env pred
     case result of
       Bool False -> (evalPs ps) env pred
       otherwise -> (evalPs ps) env conseq
evalPs ps env (List [Atom "set!",Atom var,form]) =
  evalPs ps env form >>= setVar env var
evalPs ps env (List [Atom "define",Atom var,form]) =
  evalPs ps env form >>= defineVar env var
evalPs ps env (List (Atom func:args)) =
  mapM (evalPs ps env) args >>= liftThrows . (applyPrims ps) func
evalPs _ env badForm =
  throwError $ BadSpecialForm "Unrecognized special form" badForm

apply
  :: String -> [LispVal] -> ThrowsError LispVal
apply = applyPrims []

applyPrims
  :: [Primitive] -> String -> [LispVal] -> ThrowsError LispVal
applyPrims ps f args =
  maybe (throwError $ NotFunction "Unrecognized primitive function args" f) ($ args) $
  lookup f (ps ++ primitives)