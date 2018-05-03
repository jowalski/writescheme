module LispExt where

import E2
import Data.Array
import Data.Maybe

instance Show LispVal where
  show = showVal

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList h t) = "(" ++ unwordsList h ++ " . " ++ showVal t ++ ")"
showVal (Character c) = "#\\" ++ fromMaybe [c] special
  where special = lookup c $ map flip charNameValues
        flip (a,b) = (b,a)

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal