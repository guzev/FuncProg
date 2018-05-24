{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Expressions where

import           Control.Monad.Catch  (Exception, MonadThrow, throwM)
import           Control.Monad.Reader (MonadReader, asks, liftM2,
                                       local, runReaderT)
import qualified Data.Map.Strict      as Map
import           Data.Typeable        (Typeable)

type ValMap = Map.Map String Int

data Expr = Var String
            | Lit Int
            | Add { op1 :: Expr, op2 :: Expr }
            | Sub { op1 :: Expr, op2 :: Expr }
            | Mul { op1 :: Expr, op2 :: Expr }
            | Div { op1 :: Expr, op2 :: Expr }
            | Let String Expr Expr deriving (Eq, Show)

data ExpressionError = CantFindVariable String
                       | DivisionByZero deriving (Eq, Typeable, Show)

instance Exception ExpressionError

evalImpl :: (MonadReader ValMap m, MonadThrow m) => Expr -> m Int
evalImpl (Lit n) = return n
evalImpl (Var x) = asks (Map.lookup x) >>= maybe (throwM (CantFindVariable x)) return
evalImpl (Add a b) = liftM2 (+) (evalImpl a) (evalImpl b)
evalImpl (Mul a b) = liftM2 (*) (evalImpl a) (evalImpl b)
evalImpl (Sub a b) = liftM2 (-) (evalImpl a) (evalImpl b)
evalImpl (Div a b) = do
    a1 <- evalImpl a
    b1 <- evalImpl b
    case b1 of
        0 -> throwM DivisionByZero
        _ -> return $ a1 `div` b1
evalImpl (Let name value expr) =
    evalImpl value >>= \val -> local (Map.insert name val) (evalImpl expr)

eval :: (MonadThrow m) => Expr -> ValMap -> m Int
eval expr vars = runReaderT (evalImpl expr) vars