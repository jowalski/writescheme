{-# LANGUAGE ExistentialQuantification #-}

module Prims where

import Error
import Lisp
import Control.Monad.Except
import Data.Char

-- import StrPrims
type Primitive = (String,[LispVal] -> ThrowsError LispVal)

primitives :: [Primitive]
primitives = opPrims ++ listPrims ++ eqPrims ++ stringPrims

numPrims :: (Num a
            ,Integral a)
         => [(String,a -> a -> a)]
numPrims =
  [("+",(+))
  ,("-",(-))
  ,("*",(*))
  ,("/",div)
  ,("mod",mod)
  ,("quotient",quot)
  ,("remainder",rem)]

toPrim :: ((a -> a -> b) -> [LispVal] -> ThrowsError LispVal)
       -> (String,a -> a -> b)
       -> Primitive
toPrim xBinop (s,op) = (s,xBinop op)

bbPrims :: [(String,Bool -> Bool -> Bool)]
bbPrims = [("&&",(&&)),("||",(||))]

sbPrims :: [(String,String -> String -> Bool)]
sbPrims =
  [("string=?",(==)),("string>?",(>)),("string<=?",(<=)),("string>=?",(>=))]

nbPrims :: (Ord a)
        => [(String,a -> a -> Bool)]
nbPrims = [("=",(==)),("<",(<)),(">",(>)),("/=",(/=)),(">=",(>=)),("<=",(<=))]

opPrims =
  mapToPrim numericBinop numPrims ++
  mapToPrim numBoolBinop nbPrims ++
  mapToPrim boolBoolBinop bbPrims ++ mapToPrim strBoolBinop sbPrims
  where mapToPrim xBinop = map (toPrim xBinop)

boolBinop :: (LispVal -> ThrowsError a)
          -> (a -> a -> Bool)
          -> [LispVal]
          -> ThrowsError LispVal
boolBinop unpacker op args =
  if length args /= 2
     then throwError $ NumArgs 2 args
     else do left <- unpacker $ args !! 0
             right <- unpacker $ args !! 1
             return $ Bool $ left `op` right

numBoolBinop = boolBinop unpackNum

strBoolBinop = boolBinop unpackStr

boolBoolBinop = boolBinop unpackBool

numericBinop :: (Integer -> Integer -> Integer)
             -> [LispVal]
             -> ThrowsError LispVal
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) =
  let parsed = reads n
  in if null parsed
        then throwError $ TypeMismatch "number" $ String n
        else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s) = return $ show s
unpackStr notString = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

listPrims :: [Primitive]
listPrims = [("car",car),("cdr",cdr),("cons",cons)]

car :: [LispVal] -> ThrowsError LispVal
car [List (x:xs)] = return x
car [DottedList (x:xs) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (_:xs)] = return $ List xs
cdr [DottedList [xs] x] = return x
cdr [DottedList (_:xs) x] = return $ DottedList xs x
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x1,List []] = return $ List [x1]
cons [x,List xs] = return $ List $ [x] ++ xs
cons [x,DottedList xs xlast] =
  return $
  DottedList ([x] ++ xs)
             xlast
cons [x1,x2] =
  return $
  DottedList [x1]
             x2
cons badArgList = throwError $ NumArgs 2 badArgList

eqPrims :: [Primitive]
eqPrims = [("eq?",eqv),("eqv?",eqv),("equal?",equal)]

eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1),(Bool arg2)] = return $ Bool $ arg1 == arg2
eqv [(Number arg1),(Number arg2)] = return $ Bool $ arg1 == arg2
eqv [(String arg1),(String arg2)] = return $ Bool $ arg1 == arg2
eqv [(Atom arg1),(Atom arg2)] = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x),(DottedList ys y)] =
  eqv [List $ xs ++ [x],List $ ys ++ [y]]
eqv [(List arg1),(List arg2)] =
  return $
  Bool $ (length arg1 == length arg2) && (and $ zipWith eqvPair arg1 arg2)
  where eqvPair x y =
          case eqv [x,y] of
            Left err -> False
            Right (Bool val) -> val
eqv [_,_] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList

data Unpacker =
  forall a. Eq a =>
            AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals
  :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
  (do unpacked1 <- unpacker arg1
      unpacked2 <- unpacker arg2
      return $ unpacked1 == unpacked2) `catchError`
  (const $ return False)

equal :: [LispVal] -> ThrowsError LispVal
equal [arg1,arg2] =
  do primitiveEquals <-
       liftM or $
       mapM (unpackEquals arg1 arg2)
            [AnyUnpacker unpackNum,AnyUnpacker unpackStr,AnyUnpacker unpackBool]
     eqvEquals <- eqv [arg1,arg2]
     return $
       Bool $
       (primitiveEquals ||
        let (Bool x) = eqvEquals
        in x)
equal badArgList = throwError $ NumArgs 2 badArgList

stringPrims :: [Primitive]
stringPrims =
  [("string?",isString)
  ,("make-string",makeString)
  ,("string",toString)
  ,("string-length",strLen)
  ,("string-ref",strRef)] ++
  strCIPrims

isString :: [LispVal] -> ThrowsError LispVal
isString [String _] = return $ Bool True
isString [_] = return $ Bool False
isString args = throwError $ NumArgs 2 args

makeString :: [LispVal] -> ThrowsError LispVal
makeString args =
  case args of
    [Number len] -> rep len 'X'
    [Number len,String [c]] -> rep len c
    val@(_) -> throwError $ NumArgs 2 val
  where rep i c = return $ String (replicate (fromIntegral i) c)

toString :: [LispVal] -> ThrowsError LispVal
toString cs = mapM cToStr cs >>= return . String
  where cToStr :: LispVal -> ThrowsError Char
        cToStr (String [c]) = return c
        cToStr val@(_) = throwError $ TypeMismatch "single char string" val

strLen :: [LispVal] -> ThrowsError LispVal
strLen [String s] = return . Number . fromIntegral . length $ s
strLen val@(_) = throwError $ NumArgs 1 val

strRef :: [LispVal] -> ThrowsError LispVal
strRef [String s,Number n] =
  if fromIntegral n >= length s
     then throwError $ Default "invalid index"
     else return . String $ [s !! fromIntegral n]
strRef val@([_,_]) =
  throwError $
  TypeMismatch "string, integer"
               (List val)
strRef val@(_) = throwError $ NumArgs 2 val

sbCIPrims :: [(String,String -> String -> Bool)]
sbCIPrims =
  [("string-ci=?",(==))
  ,("string-ci>?",(>))
  ,("string-ci<?",(<))
  ,("string-ci<=?",(<=))
  ,("string-ci>=?",(>=))]

unpackStrCI :: LispVal -> ThrowsError String
unpackStrCI = liftM (map toUpper) . unpackStr

strCIPrims = map (toPrim (boolBinop unpackStrCI)) sbCIPrims