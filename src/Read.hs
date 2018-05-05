module Read where

import Eval
import Env
import Lisp
import Parse (parseExpr)
import Eval (eval)
import Prims
import System.IO hiding (try)
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad.Except

readExpr :: String -> ThrowsError LispVal
readExpr input =
  case parse parseExpr "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

-- evalString :: String -> IO String
-- evalString expr =
--   return $ extractValue $ trapError (liftM show $ readExpr expr >>= eval)
-- evalAndPrint :: String -> IO ()
-- evalAndPrint expr = evalString expr >>= putStrLn
until_
  :: Monad m
  => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action =
  do result <- prompt
     if pred result
        then return ()
        else action result >> until_ pred prompt action

runRepl :: IO ()
runRepl =
  primitiveBindings >>=
  until_ (== "quit") (readPrompt "Lisp>>> ") . evalAndPrint

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

evalStrs :: Env -> [String] -> IO String
evalStrs env exprs =
  case exprs of
    [] -> return ""
    (expr:exprs) ->
      do result <- evalString env expr
         if null exprs
            then return result
            else evalStrs env exprs

primEval :: [String] -> IO String
primEval exprs = primitiveBindings >>= flip evalStrs exprs

evalString :: Env -> String -> IO String
evalString env expr =
  runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env

runOne :: String -> IO ()
runOne expr = primitiveBindings >>= flip evalAndPrint expr

primitiveBindings :: IO Env
primitiveBindings =
  nullEnv >>= (flip bindVars $ map makePrimitiveFunc primitives)
  where makePrimitiveFunc (var,func) = (var,PrimitiveFunc func)