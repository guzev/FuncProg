{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Statements where

import           Expressions         (Expr (..), ValMap, ExpressionError, eval)

import           Control.Monad       (void)
import           Control.Monad.Catch (Exception, MonadCatch, MonadThrow, catch,
                                      throwM)
import           Control.Monad.State (MonadIO, MonadState, StateT, get, liftIO,
                                      modify, put, runStateT)
import qualified Data.Map.Strict     as Map
import           Data.Typeable       (Typeable)

data StatementError = Redefinition String
                      | Undefined String
                      | EvaluationError Statement ExpressionError
                      deriving (Eq, Typeable, Show)

instance Exception StatementError

def :: (MonadState ValMap m, MonadCatch m) => String -> Int -> m ()
def name value = do
    m <- get
    case (Map.lookup name m) of
        Nothing -> put $ Map.insert name value m
        Just _  -> throwM (Redefinition name)

upd :: (MonadState ValMap m, MonadCatch m) => String -> Int -> m ()
upd name value = do
    m <- get
    case (Map.lookup name m) of
        Nothing -> throwM (Undefined name)
        Just _  -> put $ Map.insert name value m

overwrite :: (MonadState ValMap m, MonadCatch m) => String -> Int -> m ()
overwrite name value = modify $ Map.insert name value

data Statement = Def { varName :: String, expression :: Expr }
                 | Assgmnt { varName :: String, expression :: Expr }
                 | PrintVal { expression :: Expr }
                 | ReadVal { varName :: String }
                 deriving (Eq, Show)

compute :: (MonadState ValMap m, MonadCatch m, MonadIO m) => [Statement] -> m ValMap
compute [] = get
compute (x:xs) = do
    vars <- get
    c <-
        case x of
            ReadVal _ -> read <$> liftIO getLine
            _         -> evaluate x (expression x) vars
    case x of
        Def name _     -> def name c
        Assgmnt name _ -> upd name c
        PrintVal _     -> liftIO $ print c
        ReadVal name   -> overwrite name c
    compute xs
        where
          evaluate stmt expr varz = catch (eval expr varz) (\e -> throwM $ EvaluationError stmt e)


newtype StatementContext a = StatementContext { runStatement :: StateT ValMap (IO) a}
            deriving (Functor, Applicative, Monad, MonadIO,
                      MonadState ValMap, MonadThrow, MonadCatch)

execute :: StatementContext a -> IO a
execute ctx = fst <$> runStateT (runStatement ctx) Map.empty

interpret :: [Statement] -> IO ()
interpret = void . execute . compute