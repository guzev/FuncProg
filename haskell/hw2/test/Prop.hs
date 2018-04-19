module Prop where

import FirstBlock (bin)
import Hedgehog

import Data.List
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

genInt :: Gen Int
genInt = Gen.int (Range.linear 0 20)

binToIntArray :: [[Int]] -> [Int]
binToIntArray = map $ foldr (\x acc -> x + acc * 2) 0

prop_bin :: Property
prop_bin = property $
    forAll genInt >>= \i ->
    let intList = binToIntArray (bin i)
    in
      Data.List.sort(intList) === [0..(2 ^ i - 1)]