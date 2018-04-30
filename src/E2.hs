module E2 where

import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Numeric
import Data.List
import Data.Maybe
import Data.Array

data LispNumber
  = Integer Integer
  | Rational (Integer,Integer)
  | Real Float
  | Complex (LispNumber,LispNumber)
  | NA
  deriving ((((Show))))

data LispVal
  = Atom String
  | List [LispVal]
  | DottedList [LispVal]
               LispVal
  | Number LispNumber
  | Character Char
  | String String
  | Bool Bool
  deriving ((((Show))))

-- Exercises
-- 1. Rewrite parseNumber, without liftM,
--   1. using do-notation
parseNumberDo :: Parser LispVal
parseNumberDo =
  do numStr <- many1 digit
     return $ (Number . Integer . read) numStr

--   2. explicit sequencing with the >>= operator
parseNumberM :: Parser LispVal
parseNumberM = return (many1 digit) >>= liftM (Number . Integer . read)

-- 2. Our strings aren't quite R5RS compliant, because they don't support
-- escaping of internal quotes within the string. Change parseString so that \"
-- gives a literal quote character instead of terminating the string. You may
-- want to replace noneOf "\"" with a new parser action that accepts either a
-- non-quote character or a backslash followed by a quote mark.
parseStringR5 :: Parser LispVal
parseStringR5 =
  do char '"'
     x <- many (try (char '\\' >> char '\"') <|> noneOf "\"")
     char '"'
     return $ String x

-- 3. Modify the previous exercise to support \n, \r, \t, \\, and any other
-- desired escape characters
parseStringWEscape :: String -> Parser LispVal
parseStringWEscape escapes =
  do char '"'
     x <- many (try (char '\\' >> oneOf escapes) <|> noneOf "\"")
     char '"'
     return $ String x

parseStringEsc :: Parser LispVal
parseStringEsc = parseStringWEscape "\"nrt\\"

-- 4 Change parseNumber to support the Scheme standard for different
-- bases. You may find the readOct and readHex functions useful.
parseBaseNum
  :: ((String -> Integer),Parser Char) -> Parser LispVal
parseBaseNum (readFun,p) = liftM (Number . Integer . readFun) $ many1 p

parseAnyNumber :: Parser LispVal
parseAnyNumber = parseBaseNum (read,digit) <|> parseRadix

fstfst :: [(a,b)] -> a
fstfst = fst . (!! 0)

parseRadix :: Parser LispVal
parseRadix =
  do char '#'
     base <- oneOf "bodx"
     parseBaseNum
       (case base of
          'b' -> (readBinary,oneOf "01")
          'o' -> (fstfst . readOct,octDigit)
          'd' -> (read,digit)
          'x' -> (fstfst . readHex,hexDigit))

readBinary :: String -> Integer
readBinary [] = 0
readBinary (x:xs) = (read [x]) * 2 ^ (length xs) + readBinary xs

-- 5 Add a Character constructor to LispVal, and create a parser for character
-- literals as described in R5RS.
-- space, tab, line feed, form feed, and carriage return
charNameValues :: [(String,Char)]
charNameValues =
  [("space",' ')
  ,("newline",'\n')
  ,("tab",'\t')
  ,("return",'\r')
  ,("backspace",'\\')]

charNames :: Parser LispVal
charNames =
  do cn <- foldr1 (<|>) (map string (map fst charNameValues))
     return $ Character (fromJust (lookup cn charNameValues))

character :: Parser LispVal
character = liftM Character $ (letter <|> digit <|> oneOf "()")

parseCharacter :: Parser LispVal
parseCharacter = string "#\\" >> (try charNames <|> character)

-- 6 Add a Float constructor to LispVal, and support R5RS syntax for
-- decimals. The Haskell function readFloat may be useful.
-- readFloat
parseFloat :: Parser LispVal
parseFloat =
  do pre <- many (digit)
     char '.'
     post <- many (digit)
     return $ (Number . Real . fstfst . readFloat) (pre ++ ['.'] ++ post)

-- 7 Add data types and parsers to support the full numeric tower of Scheme
-- numeric types. Haskell has built-in types to represent many of these; check
-- the Prelude.  For the others, you can define compound types that represent
-- eg. a Rational as a numerator and denominator, or a Complex as a real and
-- imaginary part (each itself a Real).
parseReal :: Parser LispVal
parseReal = (try parseFloat) <|> (try parseRational) <|> parseNumberM

lispNum :: LispVal -> LispNumber
lispNum (Number x) = x
lispNum _ = NA

lispInt :: LispNumber -> Integer
lispInt (Integer x) = x
lispInt (Rational (n,m)) = n `div` m
lispInt (Real f) = round f
lispInt (Complex (r,i)) = lispInt r

parseRational :: Parser LispVal
parseRational =
  do num <- parseNumberM
     char '/'
     denom <- parseNumberM
     return $
       (Number . Rational) ((lispInt . lispNum) num,(lispInt . lispNum) denom)

parseComplex :: Parser LispVal
parseComplex =
  do real <- parseReal
     char '+'
     complex <- parseReal
     char 'i'
     return $ (Number . Complex) (lispNum real,lispNum complex)