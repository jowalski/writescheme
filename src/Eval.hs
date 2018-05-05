module Eval where

import Lisp
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
       Bool False -> (evalPs ps) env alt
       otherwise -> (evalPs ps) env conseq
evalPs ps env (List [Atom "set!",Atom var,form]) =
  evalPs ps env form >>= setVar env var
evalPs ps env (List [Atom "define",Atom var,form]) =
  evalPs ps env form >>= defineVar env var
evalPs _ env (List (Atom "define":List (Atom var:params):body)) =
  makeNormalFunc env params body >>= defineVar env var
evalPs _ env (List (Atom "define":DottedList (Atom var:params) varargs:body)) =
  makeVarargs varargs env params body >>= defineVar env var
evalPs _ env (List (Atom "lambda":List params:body)) =
  makeNormalFunc env params body
evalPs _ env (List (Atom "lambda":DottedList params varargs:body)) =
  makeVarargs varargs env params body
evalPs _ env (List (Atom "lambda":varargs@(Atom _):body)) =
  makeVarargs varargs env [] body
evalPs ps env (List (func:args)) =
  do f <- evalPs ps env func
     argVals <- mapM (evalPs ps env) args
     applyPrims ps f argVals
evalPs _ env badForm =
  throwError $ BadSpecialForm "Unrecognized special form" badForm

apply
  :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply = applyPrims []

applyPrims
  :: [Primitive] -> LispVal -> [LispVal] -> IOThrowsError LispVal
applyPrims ps (PrimitiveFunc f) args = liftThrows $ f args
applyPrims ps (Func params varargs body closure) args =
  if num params /= num args && varargs == Nothing
     then throwError $ NumArgs (num params) args
     else (liftIO $ bindVars closure $ zip params args) >>= bindVarArgs varargs >>=
          evalBody
  where remainingArgs = drop (length params) args
        num = toInteger . length
        evalBody env = liftM last $ mapM ((evalPs ps) env) body
        bindVarArgs arg env =
          case arg of
            Just argName ->
              liftIO $ bindVars env [(argName,List $ remainingArgs)]
            Nothing -> return env

makeFunc :: Maybe String
         -> Env
         -> [LispVal]
         -> [LispVal]
         -> IOThrowsError LispVal
makeFunc varargs env params body =
  return $ Func (map showVal params) varargs body env

makeNormalFunc = makeFunc Nothing

makeVarargs = makeFunc . Just . showVal