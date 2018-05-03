{-# LANGUAGE ScopedTypeVariables #-}

module PrimsTest where

import Data.List
import Test.Tasty
import Test.Tasty.HUnit
-- import Test.Tasty.Hspec
import Test.Tasty.QuickCheck
import Prims.Funs
import ReadEval
import E5

prop_sums =
  opProp "+"
         (foldl1 (+))
         (posListOf2 :: Gen [Int])

prop_subs =
  opProp "-"
         (foldl1 (-))
         (posListOf2 :: Gen [Int])

prop_prods =
  opProp "*"
         (foldl1 (*))
         (posListOf2 :: Gen [Integer])

prop_divs =
  opProp "/"
         (foldl1 div)
         (posListOf2NZ :: Gen [Integer])

prop_mods =
  opProp "mod"
         (foldl1 mod)
         (posListOf2NZ :: Gen [Integer])

prop_quots =
  opProp "quotient"
         (foldl1 quot)
         (posListOf2NZ :: Gen [Integer])

prop_rems =
  opProp "remainder"
         (foldl1 rem)
         (posListOf2NZ :: Gen [Integer])