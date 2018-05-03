module E5 where

import Prims
import Lisp
import Error
import Eval (evalPs, eval)
import Control.Monad
import Control.Monad.Except
import Data.Either
import Data.Char
import Env

-- 1. Instead of treating any non-false value as true, change the definition of
-- if so that the predicate accepts only Bool values and throws an error on any
-- others.
evalPs2
  :: [Primitive] -> Env -> LispVal -> IOThrowsError LispVal
evalPs2 ps env (List [Atom "if",pred,conseq,alt]) =
  do result <- (evalPs2 ps env) pred
     case result of
       Bool False -> (evalPs2 ps env) pred
       Bool True -> (evalPs2 ps env) conseq
       otherwise@(_) -> throwError $ TypeMismatch "boolean" otherwise
evalPs2 ps env lv = evalPs ps env lv

-- 2. equal? has a bug in that a list of values is compared using eqv?  instead
-- of equal?. For example, (equal? ’(1 "2") ’(1 2)) = #f, while you’d expect it
-- to be true. Change equal? so that it continues to ignore types as it recurses
-- into list structures. You can either do this explicitly, following the
-- example in eqv?, or factor the list clause into a separate helper function
-- that is parameterized by the equality testing function.
equal2 :: [LispVal] -> ThrowsError LispVal
equal2 [(List arg1),(List arg2)] =
  return $
  Bool $ (length arg1 == length arg2) && (and $ zipWith equalPair arg1 arg2)
  where equalPair x y =
          case equal [x,y] of
            Left err -> False
            Right (Bool val) -> val
equal2 xs = equal xs

-- 3. Implement cond and case expressions.
evalExprs
  :: Env -> [LispVal] -> IOThrowsError LispVal
evalExprs env [] = throwError $ NumArgs 1 []
evalExprs env (expr:exprs) =
  do result <- eval env expr
     if null exprs
        then return result
        else evalExprs env exprs

cond :: Env -> [LispVal] -> IOThrowsError LispVal
cond env [List ((Atom "else"):exprs)] = evalExprs env exprs
cond env ((List (test:exprs)):otherClauses) =
  do result <- eval env test
     case result of
       Bool False -> cond env otherClauses
       val@(otherVal) ->
         case exprs of
           [(Atom "=>"),Atom func] -> eval env (List ((Atom func) : [val]))
           (expr:restExprs) -> evalExprs env exprs
           [] -> return val
cond env [] = return $ List []
cond env badArgList =
  throwError $
  BadSpecialForm "cond"
                 (List badArgList)

caseL
  :: Env -> [LispVal] -> IOThrowsError LispVal
caseL env (key:clauses) =
  do result <- eval env key
     clauseExprs <- firstMatch result clauses
     if null clauseExprs
        then return (List [])
        else evalExprs env clauseExprs
  where firstMatch
          :: LispVal -> [LispVal] -> IOThrowsError [LispVal]
        firstMatch key [List (Atom "else":exprs)] = return exprs
        firstMatch key ((List ((List datum):exprs)):cs) =
          do if any (eqvPair key) datum
                then return exprs
                else firstMatch key cs
        firstMatch key [] = return []
        firstMatch key val@(_) =
          throwError $
          BadSpecialForm "case clause"
                         (List val)
        eqvPair x y =
          case eqv [x,y] of
            Left err -> False
            Right (Bool val) -> val

-- 4. Add the rest of the string functions. You don’t yet know enough to do
-- string-set!; this is difficult to implement in Haskell, but you’ll have
-- enough information after the next two sections.
-- (see strPrims)
blah = id