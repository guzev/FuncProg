 module ExpressionsTests where

import           Expressions

import qualified Data.Map.Strict as Map
import           Test.Hspec

spec :: Spec
spec = do
    let map1 = Map.singleton "x" 1
    let map0 = Map.empty
    it "literal" $ do
        eval (Lit 1) map1 `shouldReturn` 1
    it "variable" $ do
        eval (Var "x") map1 `shouldReturn` 1
        eval (Var "y") map1 `shouldThrow` (== CantFindVariable "y")
    it "add" $ do
        eval (Lit 2 `Add` Lit 2) map0 `shouldReturn` 4
    it "div" $ do
        eval (Var "x" `Div` Lit 0) map1 `shouldThrow` (== DivisionByZero)
        eval (Lit 5 `Div` Lit 2) map0 `shouldReturn` 2
    it "let" $ do
        eval ("y" `Let` (Lit 2) $ Var "x" `Add` Var "y") map1 `shouldReturn` 3
        eval (Var "x" `Add` (Lit 3 `Mul` ("x" `Let` (Lit 2) $ Var "x"))) map1 `shouldReturn`
            7
        eval (Var "y" `Add` (Lit 3 `Mul` ("x" `Let` (Lit 2) $ Var "x"))) map1 `shouldThrow`
            (== CantFindVariable "y")
        eval
            (Let
                 ("y")
                 (Lit 2 `Add` Lit 2)
                 (Var "y" `Add` (Lit 3 `Mul` ("x" `Let` (Lit 2) $ Var "x"))))
            map0 `shouldReturn`
            10