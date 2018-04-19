module Unit 
       ( hspecTestTree,
       ) where

import Data.Maybe (isJust, isNothing)

import Test.Tasty (TestTree)
import Test.Tasty.Hspec (Spec, describe, it, shouldBe, shouldSatisfy, testSpec)

import FirstBlock (ArithmeticError (..), Expr (..), eval)
import SecondBlock (stringSum)

hspecTestTree :: IO TestTree
hspecTestTree = testSpec "HW2" spec_arithmetic

spec_arithmetic :: Spec
spec_arithmetic = do
  describe "arithmetic" $ do
    it "computation" $ do
      eval (Add (Const 4) (Const 5)) `shouldSatisfy` (==Right 9)
    it "div by zero" $ do
      eval (Div (Const 5) (Const 0)) `shouldSatisfy` (==Left DivisionByZero)
    it "negative power" $ do
      eval (Pow (Const 4) (Const (-4))) `shouldSatisfy` (==Left NegativeExponent)
  describe "stringsum" $ do
    it "correct" $ do
      stringSum "1         2 100     \t\n4 5" `shouldSatisfy` (== Just 112)
    it "mistakes" $ do
      stringSum "hmmm smth goes wrong" `shouldSatisfy` isNothing
    it "empty" $ do
      stringSum "" `shouldSatisfy` (== Just 0)