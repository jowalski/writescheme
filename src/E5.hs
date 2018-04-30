module E5 where

import Prims
import Lisp
import Error
import Eval
import ReadEval
import Read
import Control.Monad.Except

-- 1. Instead of treating any non-false value as true, change the definition of
-- if so that the predicate accepts only Bool values and throws an error on any
-- others.
evalPs2
  :: [Primitive] -> LispVal -> ThrowsError LispVal
evalPs2 ps (List [Atom "if",pred,conseq,alt]) =
  do result <- (evalPs2 ps) pred
     case result of
       Bool False -> (evalPs2 ps) pred
       Bool True -> (evalPs2 ps) conseq
       otherwise@(_) -> throwError $ TypeMismatch "boolean" otherwise
evalPs2 ps lv = evalPs ps lv

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
cond :: [LispVal] -> ThrowsError LispVal
cond [List ((Atom "else"):exprs)] = last $ map eval exprs
cond ((List (test:exprs)):otherClauses) =
  do result <- eval test
     case result of
       Bool False -> cond otherClauses
       val@(otherVal) ->
         case exprs of
           [(Atom "=>"),func] -> eval (List [func,val])
           (expr:restExprs) -> last $ map eval exprs
           [] -> return val
cond [] = return $ List []
cond badArgList =
  throwError $
  BadSpecialForm "cond"
                 (List badArgList)

testCond :: [LispVal] -> ThrowsError LispVal
testCond ((List (test:exprs)):otherClauses) = return $ String "blahblah"
testCond _ = return $ String "no"

-- 4.
blah = id