{-# LANGUAGE InstanceSigs #-}

module Task5 where

import Data.Monoid (Sum (..))
import Data.Semigroup (Max (..), Semigroup (..))
import Task4 (NonEmpty ((:|)))

maybeConcat :: [Maybe [a]] -> [a]
maybeConcat = foldr my_concat []
            where
              my_concat Nothing acc     = acc
              my_concat (Just list) acc = list ++ acc

eitherConcat :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
eitherConcat = foldr my_concat (mempty, mempty)
            where
              my_concat (Left x) (accl, accr)  = (x `mappend` accl, accr)
              my_concat (Right x) (accl, accr) = (accl, x `mappend` accr)

instance Semigroup (NonEmpty t) where
    (<>) (x :| xs) (y :| ys) = x :| (xs ++ (y:ys))

data ThisOrThat a b = This a | That b | Both a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (ThisOrThat a b) where
    (<>) (This a) (This b)     = This (a <> b)
    (<>) (That a) (That b)     = That (a <> b)
    (<>) (This a) (That b)     = Both a b
    (<>) (Both a b) (This c)   = Both (a <> c) b
    (<>) (Both a b) (That c)   = Both a (b <> c)
    (<>) (Both a b) (Both c d) = Both (a <> c) (b <> d)
    (<>) (This c) (Both a b)   = Both (c <> a) b
    (<>) (That c) (Both a b)   = Both a (c <> b)
    (<>) x y                   = y <> x -- That <> This

data Builder = One Char | Many [Builder] deriving (Show, Eq)

instance Semigroup Builder where
    (<>) x (Many [])         = x
    (<>) (Many []) y         = y
    (<>) a@(One _) b@(One _) = Many [a, b]
    (<>) (Many xs) (Many ys) = Many (xs ++ ys)
    (<>) a@(One _) (Many ys) = Many $ a:ys
    (<>) (Many xs) b@(One _) = Many (xs ++ [b])

instance Monoid Builder where
    mempty = Many []
    mappend = (<>)

foldrWithCtor :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldrWithCtor ctor = foldr (mappend . ctor) mempty

fromString :: String -> Builder
fromString = foldrWithCtor One

toString :: Builder -> String
toString (One ch)   = [ch]
toString (Many chs) = foldrWithCtor toString chs

-- tests

checkConcat :: Bool
checkConcat = maybeConcat [Just[1,2,3], Nothing,
                           Just[4,5,6], Nothing] == [1,2,3,4,5,6] &&
              eitherConcat [Left (Sum 3), Right [1,2,3], Left (Sum 5), Right [4,5]] ==
                            (Sum {getSum = 8}, [1,2,3,4,5])

checkThisOrThat :: Bool
checkThisOrThat = (This $ Sum 4) <> Both (Sum 5) (Max 10) <> (That $ Max 20) ==
                                Both (Sum 9) (Max 20)

checkBuilder :: Bool
checkBuilder = let str = "abacabadab" in toString (fromString str) == str


testTask5 :: [Bool]
testTask5 = [checkConcat, checkThisOrThat, checkBuilder]

checkTask5 :: [Int]
checkTask5 = map fst $ filter (not . snd) $ zip [1..] testTask5