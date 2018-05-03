{-# LANGUAGE ScopedTypeVariables #-}

module E5Test where

import Data.List
import Test.Tasty
import Test.Tasty.HUnit
-- import Test.Tasty.Hspec
-- import Test.Tasty.QuickCheck
import Prims.Funs
import ReadEval
import E5

-- evalCond = evalStringNEnv [("cond",cond)]
-- unit_condFirstClause :: IO ()
-- unit_condFirstClause =
--   evalCond "(cond '((> 3 2) 'greater) '((< 3 2) 'less))" @?= "greater"
-- unit_condOtherClause :: IO ()
-- unit_condOtherClause =
--   evalCond "(cond '((> 3 4) 'greater) '((< 3 4) 'less))" @?= "less"
-- unit_condElseClause :: IO ()
-- unit_condElseClause =
--   evalCond "(cond '((> 3 3) 'greater) '((< 3 3) 'less) '(else 'equal))" @?=
--   "equal"
-- unit_condArrow :: IO ()
-- unit_condArrow = evalCond "(cond '((cdr '(1 2)) => car) '(else 'else))" @?= "2"
-- evalCase = evalStringNEnv [("case",caseL)]
-- unit_caseFirstClause :: IO ()
-- unit_caseFirstClause =
--   evalCase "(case (* 2 3) '((1 4 6) 'composite) '((2 3 5 7) 'prime) '(else 'other))" @?=
--   "composite"
-- unit_caseOtherClause :: IO ()
-- unit_caseOtherClause =
--   evalCase "(case (* 2 1) '((1 4 6) 'composite) '((2 3 5 7) 'prime) '(else 'other))" @?=
--   "prime"
-- unit_caseElseClause :: IO ()
-- unit_caseElseClause =
--   evalCase "(case (* 2 4) '((1 4 6) 'composite) '((2 3 5 7) 'prime) '(else 'other))" @?=
--   "other"
lspEq expr result = evalStringNEnv stringPrims expr >>= (@?=) result

-- unit_stringQTrue :: IO ()
unit_stringQTrue = "(string? \"hello\")" `lspEq` "#t"

-- do x <- evalStrPrims "(string? \"hello\")"
--    return $ x @?= "#t"
unit_stringQFalse = "(string? 3)" `lspEq` "#f"

unit_makeString = "(make-string 3 \"A\")" `lspEq` "\"AAA\""

-- "string","string-length","string-ref","string-ci<?"
unit_stringStr = "(string \"a\" \"b\" \"c\")" `lspEq` "\"abc\""

unit_stringLen = "(string-length \"xyzABC\")" `lspEq` "6"

unit_strRef = "(string-ref \"xyzABC\" 3)" `lspEq` "\"A\""

unit_strCIEQTrue = "(string-ci=? \"abc\" \"ABC\")" `lspEq` "#t"

unit_strCIEQFalse = "(string-ci=? \"abc\" \"abd\")" `lspEq` "#f"

unit_strCILTTrue = "(string-ci<? \"abc\" \"ABD\")" `lspEq` "#t"

unit_strCILTFalse = "(string-ci<? \"ABC\" \"abb\")" `lspEq` "#f"

unit_strCIGTTrue = "(string-ci>? \"abd\" \"ABC\")" `lspEq` "#t"

unit_strCIGTFalse = "(string-ci>? \"abb\" \"ABC\")" `lspEq` "#f"

unit_strCILTETrue = "(string-ci<=? \"abd\" \"ABD\")" `lspEq` "#t"

unit_strCILTEFalse = "(string-ci<=? \"ABC\" \"abb\")" `lspEq` "#f"

unit_strCIGTETrue = "(string-ci>=? \"abc\" \"ABC\")" `lspEq` "#t"

unit_strCIGTEFalse = "(string-ci>=? \"abb\" \"ABC\")" `lspEq` "#f"