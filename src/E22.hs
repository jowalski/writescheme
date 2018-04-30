module E22 where

import Parse (parseExpr, spaces)
import Lisp
-- import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
-- import Numeric
-- import Data.List
-- import Data.Maybe
import Data.Array

-- 1 Add support for the backquote syntactic sugar: the Scheme standard details
-- what it should expand into (quasiquote/unquote).
parseQ :: String -> String -> Parser LispVal
parseQ sym label =
  do string sym
     x <- parseExpr
     return $ List [Atom label,x]

parseQuoted :: Parser LispVal
parseQuoted =
  do parseQ "`" "quasiquote" <|> try (parseQ ",@" "unquote-splicing") <|>
       parseQ "," "unquote"

-- 2 Add support for vectors. The Haskell representation is up to you: GHC does
-- have an Array data type, but it can be difficult to use.  Strictly speaking,
-- a vector should have constant-time indexing and updating, but destructive
-- update in a purely functional language is difficult. You may have a better
-- idea how to do this after the section on set!, later in this tutorial.
parseVector :: Parser LispVal
parseVector =
  do char '#'
     char '('
     elems <- sepBy parseExpr spaces
     char ')'
     return $ Array (listArray (0,length elems - 1) elems)

--
-- 3. Instead of using the try combinator, left-factor the grammar so that the
-- common subsequence is its own parser. You should end up with a parser that
-- matches a string of expressions, and one that matches either nothing or a dot
-- and a single expression.  Combining the return values of these into either a
-- List or a DottedList is left as a (somewhat tricky) exercise for the reader:
-- you may want to break it out into another helper function.
blah = id