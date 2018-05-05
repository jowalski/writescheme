module Lisp where

import Data.Array
import Data.IORef
import Text.Parsec
import Control.Monad.Except

data LispVal
  = Atom String
  | List [LispVal]
  | DottedList [LispVal]
               LispVal
  | Number Integer
  | String String
  | Bool Bool
  | Array (Array Int LispVal)
  | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
  | Func {params :: [String]
         ,vararg :: (Maybe String)
         ,body :: [LispVal]
         ,closure :: Env}

instance Show LispVal where
  show = showVal

type Env = IORef [(String,IORef LispVal)]

type ThrowsError = Either LispError-- instance Except LispError

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList h t) = "(" ++ unwordsList h ++ " . " ++ showVal t ++ ")"
showVal (PrimitiveFunc _) = "<primitive>"
showVal (Func{params = args,vararg = varargs,body = body,closure = env}) =
  "(lambda (" ++
  unwords (map show args) ++
  (case varargs of
     Nothing -> ""
     Just arg -> " . " ++ arg) ++
  ") ...)"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

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

trapError action =
  catchError action
             (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

type IOThrowsError = ExceptT LispError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runExceptT (trapError action) >>= return . extractValue