{-# LANGUAGE TemplateHaskell #-}

module PrimTests where

import Test.QuickCheck
import Test.QuickCheck.All
import Test.QuickCheck.Monadic
import Data.Either
import ReadEval
import Lisp

-- It’s always more efficient to use a custom generator than to conditionally
-- discard test cases. Here’s generator for messages in the Base64 alphabet with
-- proper equals padding, plus the modified test.
lispOpList :: Show a
           => String -> [a] -> String
lispOpList op ns = "(" ++ op ++ " " ++ (unwords . map show) ns ++ ")"

listOf2 :: Arbitrary a
        => Gen [a]
listOf2 = ((:) <$> arbitrary <*> listOf1 arbitrary)

arbSt :: Arbitrary a
      => (a -> Bool) -> Gen a
arbSt = (suchThat) arbitrary

posListOf2 :: (Arbitrary a
              ,Integral a)
           => Gen [a]
posListOf2 = ((:) <$> arbPos <*> listOf1 arbPos)
  where arbPos = arbSt (>= 0)

posListOf2NZ :: (Arbitrary a
                ,Integral a)
             => Gen [a]
posListOf2NZ = ((:) <$> arbSt (>= 0) <*> listOf1 (arbSt (> 0)))

opProp
  :: (Show a
     ,Show b)
  => String -> ([a] -> b) -> Gen [a] -> Property
opProp lispOp hFun gen =
  forAll gen $
  \xs ->
    (either (const "")
            id
            ((readEval . lispOpList lispOp) xs)) ==
    (show . hFun) xs

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

-- prop_strEq =
--   opProp "string=?"
--          (\[x,y] ->
--             if x == y
--                then "#t"
--                else "#f")
--          ((vectorOf 2 arbitrary) :: Gen [String])
return []

runTests :: IO Bool
runTests = $quickCheckAll