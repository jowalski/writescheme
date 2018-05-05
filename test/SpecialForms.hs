{-# LANGUAGE ScopedTypeVariables #-}

module SpecialForms where

import Data.List
import Test.Tasty
import Test.Tasty.HUnit
-- import Test.Tasty.Hspec
import Test.Tasty.QuickCheck
import Prims.Funs
-- import ReadEval
import Read

lspEq expr result = primEval expr >>= (@=?) result

unit_defineFun = ["(define (f x y) (+ x y))","(f 1 2)"] `lspEq` "3"

unit_defineVar = ["(define x 10)","x"] `lspEq` "10"

unit_setVar = ["(define x 10)","(set! x 11)","x"] `lspEq` "11"

unit_defineRecursiveFun =
  ["(define (factorial x) (if (= x 1) 1 (* x (factorial (- x 1)))))"
  ,"(factorial 10)"] `lspEq`
  "3628800"

unit_defineLambda =
  ["(define (counter inc) (lambda (x) (set! inc (+ x inc)) inc))"
  ,"(define my-count (counter 5))"
  ,"(my-count 3)"
  ,"(my-count 6)"] `lspEq`
  "14"