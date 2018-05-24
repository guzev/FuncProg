{-# LANGUAGE OverloadedStrings #-}

module Parser where

import           Expressions                (Expr (..), ValMap, eval)
import           Statements                 (Statement (..), compute, interpret)

import           Control.Applicative        (empty)
import           Control.Monad.Catch        (Exception, MonadCatch, MonadThrow,
                                             throwM)
import           Control.Monad.State        (MonadIO, runStateT)
import qualified Data.Map.Strict            as Map
import           Data.Typeable              (Typeable)
import           Data.Void                  (Void)

import qualified Data.ByteString            as PackedStr
import qualified Data.ByteString.Internal   as BS (c2w)
import qualified Data.ByteString.UTF8       as S8

import           Text.Megaparsec
import           Text.Megaparsec.Byte       (alphaNumChar, char, eol,
                                             letterChar, string)
import qualified Text.Megaparsec.Byte.Lexer as L
import           Text.Megaparsec.Expr

type Str = S8.ByteString

type Parser = Parsec Void Str

data ParsingException = ParsingException (ParseError (Token Str) Void) deriving (Typeable, Show)

instance Exception ParsingException

space1 :: Parser ()
space1 = skipSome (char (BS.c2w ' '))

indent :: Parser ()
indent = skipMany (char (BS.c2w ' '))

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Str -> Parser Str
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

integer :: Parser Int
integer = lexeme L.decimal

rightWord :: Str -> Parser ()
rightWord w = lexeme (string w *> notFollowedBy alphaNumChar)

rws :: [Str]
rws = ["let", "mut"]

identifier :: Parser Str
identifier = (lexeme . try) (p >>= check)
  where
    p       = PackedStr.pack <$> ((:) <$> letterChar <*> many alphaNumChar)
    check x = if x `elem` rws
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x

term :: Parser Expr
term = parens exprParser
    <|> (Let <$> (symbol "(" *> rightWord "let" *> (S8.toString <$> identifier) <* symbol "=")
            <*> (exprParser <* symbol "in" ) <*> (exprParser <* symbol ")"))
    <|> Var <$> (S8.toString <$> identifier)
    <|> Lit <$> integer

operators :: [[Operator Parser Expr]]
operators =
        [ [
            InfixL (Mul <$ symbol "*")
            , InfixL (Div <$ symbol "/")
        ]
        , [
            InfixL (Add <$ symbol "+")
            , InfixL (Sub <$ symbol "-")
        ]
        ]

exprParser :: Parser Expr
exprParser = makeExprParser term operators

stmtParser :: Parser Statement
stmtParser = indent *> (
          Def <$> (rightWord "mut" *> (S8.toString <$> identifier) <* symbol "=") <*> exprParser
          <|> PrintVal <$> (symbol "<" *> exprParser)
          <|> ReadVal <$> (symbol ">" *> (S8.toString <$> identifier))
          <|> Assgmnt <$> ((S8.toString <$> identifier) <* symbol "=") <*> exprParser
        )

programParser :: Parser [Statement]
programParser = sc *> many (stmtParser <* eol)

useParser :: (MonadThrow m) => Parser a -> String -> Str -> m a
useParser p name input = either (throwM . ParsingException) return (parse p name input)

parseExprs :: (MonadThrow m) => Str -> m Expr
parseExprs = useParser exprParser ""

parseAndEval :: (MonadThrow m) => Str -> m Int
parseAndEval input = parseExprs input >>= flip eval Map.empty

parseAndCompute :: (MonadIO m, MonadCatch m) => Str -> m ValMap
parseAndCompute input = useParser stmtParser "" input >>= \stmt -> fst <$> runStateT (compute [stmt]) Map.empty

runProgram :: String -> Str -> IO ()
runProgram name input = useParser programParser name input >>= interpret