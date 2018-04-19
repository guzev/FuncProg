{-# LANGUAGE InstanceSigs #-}
module SecondBlock where

import           Data.Foldable (toList)
import           Text.Read     (readMaybe)

-- task1

stringSum :: String -> Maybe Int
stringSum str = fmap sum $ sequence $ readMaybe <$> words str

-- task2
-- prove of Optional is located in Optional.txt

newtype Optional a = Optional (Maybe (Maybe a)) deriving Show

instance Functor Optional where
    fmap _ (Optional Nothing)                = Optional Nothing
    fmap _ (Optional (Just Nothing))         = Optional $ Just Nothing
    fmap function (Optional (Just (Just x))) = Optional $ Just $ Just $ function x

instance Applicative Optional where
    pure x = Optional $ Just $ Just x

    (Optional Nothing) <*> _                  = Optional Nothing
    (Optional (Just Nothing)) <*> _           = Optional $ Just Nothing
    (Optional (Just (Just function))) <*> op  = fmap function op

instance Monad Optional where
    return = pure

    Optional Nothing >>= _                = Optional Nothing
    Optional (Just Nothing) >>= _         = Optional (Just Nothing)
    Optional (Just (Just a)) >>= function = function a

instance Foldable Optional where
    foldr _ z (Optional Nothing)                = z
    foldr _ z (Optional (Just Nothing))         = z
    foldr function z (Optional (Just (Just a))) = function a z

instance Traversable Optional where
    traverse _ (Optional Nothing)                = pure $ Optional Nothing
    traverse _ (Optional (Just Nothing))         = pure $ Optional $ Just Nothing
    traverse function (Optional (Just (Just a))) = Optional . Just . Just <$> function a

-- task3

data NonEmpty a = a :| [a] deriving Show

instance Functor NonEmpty where
    fmap function (x:|xs) = function x :| map function xs

instance Applicative NonEmpty where
    pure a = a:|[]

    (function:|fs) <*> (x:|xs) = function x :| drop 1 [fs' xs' | fs' <- function:fs, xs' <- x:xs]

instance Monad NonEmpty where
    return = pure

    (x:|xs) >>= function = y :| (ys ++ ys')
      where
        y :| ys = function x
        ys' = xs >>= toList . function

instance Foldable NonEmpty where
    foldr function z (x:|xs) = x `function` foldr function z xs

instance Traversable NonEmpty where
    traverse function (x:|xs) = (:|) <$> function x <*> traverse function xs