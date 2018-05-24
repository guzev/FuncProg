{-# LANGUAGE OverloadedStrings #-}

module ParserTests where

import           Expressions
import           Parser
import           Statements

import qualified Data.Map.Strict      as Map
import           Test.Hspec

spec :: Spec
spec = do
    it "2 + 2" $ do
        parseExprs "2 + 2" `shouldReturn` (Lit (2::Int)) `Add` (Lit 2)
        parseAndEval "2 + 2" `shouldReturn` 4
    it "x + 2" $ do
        parseAndEval "x + 2" `shouldThrow` (== CantFindVariable "x")
    it "1+  (     let x  =2 in x   )  " $ do
        parseAndEval "1+  (     let x  =2 in x   )  " `shouldReturn` 3
    it "10 + 3 * (let x = 2 in x)" $ do
        parseAndEval "10 * 3 * (let x = 2 in x)" `shouldReturn` 60
    it "mut x = 2 * 2 * 2" $ do
        parseAndCompute "mut x = 2 + 2 * 2" `shouldReturn` Map.singleton "x" 8
    it "x = 2 + 2 * 2" $ do
        parseAndCompute "x = 2 + 2 * 2" `shouldThrow` (== Undefined "x")