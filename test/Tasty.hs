{-# LANGUAGE ScopedTypeVariables #-}

module Tasty where

import Data.List
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hspec
import Test.Tasty.QuickCheck

-- HUnit test case
unit_listCompare :: IO ()
unit_listCompare = [1,2,3] `compare` [1,2] @?= GT

-- QuickCheck property
prop_additionCommutative :: Int -> Int -> Bool
prop_additionCommutative a b = a + b == b + a

-- SmallSheck property
scprop_sortReverse :: [Int] -> Bool
scprop_sortReverse list = sort list == sort (reverse list)

-- Hspec specification
spec_prelude :: Spec
spec_prelude =
  do describe "Prelude.head" $
       do it "returns the first element of a list" $
            do head [23 ..] `shouldBe` (23 :: Int)

-- Tasty TestTree
test_multiplication :: [TestTree]
test_multiplication =
  [testProperty "One is identity" $ \(a :: Int) -> a * 1 == a]

-- Tasty IO TestTree
test_generateTree :: IO TestTree
test_generateTree =
  do input <- pure "Some input"
     pure $ testCase input $ pure ()

-- Tasty IO [TestTree]
test_generateTrees :: IO [TestTree]
test_generateTrees =
  do inputs <- pure ["First input","Second input"]
     pure $ map (\s -> testCase s $ pure ()) inputs