module Prims.Funs where

import Test.QuickCheck
import Test.QuickCheck.Monadic
import ReadEval

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
    monadicIO $
    do result <- run $ evalStringNEnv [] . lispOpList lispOp $ xs
       assert $ result == (show . hFun) xs