module Task2 where

import Data.Maybe
import System.Random (newStdGen, randomRs)

randomIntList :: Int -> Int -> Int -> IO [Int]
randomIntList n from to = take n . randomRs (from, to) <$> newStdGen

remove :: Int -> [a] -> Maybe a
remove i list = snd (remove' i list)

remove' :: Int -> [a] -> ([a], Maybe a)
remove' _ [] = ([], Nothing)
remove' 0 (x:xs) = (xs, Just x)
remove' i (x:xs) = (x : fst rem, snd rem) where
    rem = remove' (i - 1) xs

merge :: Ord a => [a] -> [a] -> [a]
merge x []              = x
merge [] y              = y
merge (x : xs) (y : ys) = if x < y then x : (merge xs (y : ys)) else y : (merge (x : xs) ys)

split :: [a] -> ([a], [a]) -> ([a], [a])
split [] (y, z)             = (y, z)
split (x : []) (y, z)       = (x : y, z)
split (xf : xs : x) (y, z)  = split x (xf : y, xs : z) 


mergeSort :: Ord a => [a] -> [a]
mergeSort []           = []
mergeSort (x : [])     = x : []
mergeSort (f : s : []) = if f < s then f : s : [] else s : f : []
mergeSort list         = let p = split list ([], []) 
                              in merge (mergeSort (fst p)) (mergeSort (snd p)) 

-- tests

checkRemove :: Bool
checkRemove = Task2.remove 5 [1, 2, 3, 4, 5, 6] == Just 6 &&
                Task2.remove 6 [1, 2, 3, 4, 5, 6] == Nothing

checkMergeSort :: Bool
checkMergeSort = mergeSort [5, 1, 2, 10, 3, 4, -5] == [-5, 1, 2, 3, 4, 5, 10]
