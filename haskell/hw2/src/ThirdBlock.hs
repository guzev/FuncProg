module ThirdBlock where

import Control.Applicative (Alternative, empty, (<|>))
import Control.Monad (join, (>=>))
import Data.Bifunctor (first)

-- task1

newtype Parser s a = Parser {runParser :: [s] -> Maybe (a, [s]) }

instance Functor (Parser s) where
    fmap f (Parser parser) = Parser (fmap (first f) . parser)

instance Applicative (Parser s) where
    pure x = Parser $ \s -> Just (x, s)
    Parser pf <*> Parser pa = Parser $ pf >=> \(f, s) ->
                                       pa s >>= \(a, left) ->
                                       Just (f a, left)

instance Monad (Parser s) where
    return = pure
    (Parser parser) >>= f = Parser (\s -> join $ (uncurry runParser) <$> (first f <$> parser s))

instance Alternative (Parser s) where
    empty                     = Parser (const Nothing)
    (Parser a) <|> (Parser b) = Parser (\s -> (a s) <|> (b s))