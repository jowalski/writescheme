module Error where

import Lisp
import Text.Parsec
import Control.Monad.Except

data LispError
  = NumArgs Integer
            [LispVal]
  | TypeMismatch String
                 LispVal
  | Parser ParseError
  | BadSpecialForm String
                   LispVal
  | NotFunction String
                String
  | UnboundVar String
               String
  | Default String

showError :: LispError -> String
showError (UnboundVar m var) = m ++ ": " ++ var
showError (BadSpecialForm m form) = m ++ ": " ++ show form
showError (NotFunction m func) = m ++ ": " ++ show func
showError (NumArgs expected found) =
  "Expected " ++ show expected ++ " args: found values " ++ unwordsList found
showError (TypeMismatch expected found) =
  "Invalid type: expected " ++ expected ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr

instance Show LispError where
  show = showError

type ThrowsError = Either LispError-- instance Except LispError

-- trapError :: (MonadError a m
--              ,Show a)
--           => m String -> m String
trapError action =
  catchError action
             (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val