module Main where

import Hedgehog   (check)
import Test.Tasty (defaultMain, testGroup)

import Unit  (hspecTestTree)
import Prop  (prop_bin)

main :: IO()
main = do
        _ <- test2
        test1
        

test1 :: IO ()
test1 = hspecTestTree >>= \unitTests ->
          let allTests = testGroup "all tests" [unitTests]
          in defaultMain allTests

test2 :: IO Bool
test2 = check prop_bin