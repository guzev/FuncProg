module Task1 where

import Data.List

order3 :: (Ord a) => (a, a, a) -> (a, a, a)
order3 (a, b, c) = (x, y, z) where [x, y, z] = sort [a, b, c]

smartReplicate :: [Int] -> [Int]
smartReplicate [] = []
smartReplicate (x:xs) = replicate x x ++ smartReplicate xs

contains :: Int -> [[Int]] -> [[Int]]
contains x list = [y | y <- list, elem x y]

stringSum :: String -> Int
stringSum str = sum (map read (words str))

--tests

checkOrder :: Bool
checkOrder = order3 (3, 2, 1) == (1, 2, 3) && order3 (3, 1, 2) == (1, 2, 3)

checkSmartReplicate :: Bool
checkSmartReplicate = smartReplicate [3, 2, 1] == [3, 3, 3, 2, 2, 1]

checkContains :: Bool
checkContains = contains 1 [[1, 2, 3], [6, 2, 9], [], [2, 3, 4, 10], [1]] == [[1, 2, 3], [1]]

checkStringSum :: Bool
checkStringSum = stringSum " 1\t\n 2 5 10 " == 18