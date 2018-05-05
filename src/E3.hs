module E3 where

import Lisp
import Eval
import Read
import Prims
import Control.Monad.Except

-- 1. Add primitives to perform the various type-testing functions of R5RS:
typeTestPrimitives :: [Primitive]
typeTestPrimitives =
  [("symbol?",isTH "atom")
  ,("string?",isTH "string")
  ,("number?",isTH "number")
  ,("char?",isTH "atom")
  ,("vector?",isTH "vector")
  ,("procedure?",isTH "procedure")
  ,("bool?",isTH "bool")
  ,("pair?",isTH "dotlist")
  ,("null?",isTH "null")
  ,("list?",isTH "list")]
  where isTH s = oneArgPrim $ (return . isT s)

oneArgPrim :: (LispVal -> ThrowsError LispVal)
           -> [LispVal]
           -> ThrowsError LispVal
oneArgPrim f lvs =
  do case lvs of
       [x] -> f x
       notOneArg@(_) -> throwError $ NumArgs 1 notOneArg

-- oneArgPrim f [x] = return $ f x
-- oneArgPrim f multArgs@(_) = throwError $ NumArgs 1 multArgs
isT :: String -> LispVal -> LispVal
isT "atom" (Atom _) = Bool True
isT "null" (List []) = Bool True
isT "list" (List _) = Bool True
isT "dotlist" (DottedList _ _) = Bool True
isT "number" (Number _) = Bool True
isT "string" (String _) = Bool True
isT "bool" (Bool _) = Bool True
isT "vector" (Array _) = Bool True
isT _ _ = Bool False

-- evalPs typeTestPrimitives (readExpr "(pair? '(a . c)')")
-- ...
-- 2. Change unpackNum so that it always returns 0 if the value is not a number,
-- even if it's a string or list that could be parsed as a number.
unpackNum2 :: LispVal -> Integer
unpackNum2 (Number n) = n
unpackNum2 _ = 0

-- 3. Add the symbol-handling functions from R5RS. A symbol is what we've been
-- calling an Atom in our data constructors.
symToStr :: LispVal -> ThrowsError LispVal
symToStr (Atom s) = return $ String s
symToStr otherType@(_) = throwError $ TypeMismatch "symbol" otherType

strToSym :: LispVal -> ThrowsError LispVal
strToSym (String s) = return $ Atom s
strToSym otherType@(_) = throwError $ TypeMismatch "string" otherType

symbolPrimitives :: [Primitive]
symbolPrimitives =
  [("symbol->string",oneArgPrim symToStr)
  ,("string->symbol",oneArgPrim strToSym)]