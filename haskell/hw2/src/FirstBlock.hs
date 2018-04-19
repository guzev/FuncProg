module FirstBlock where

import Control.Monad

-- task1

data Expr = Const Int
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Pow Expr Expr deriving (Eq, Show)

data ArithmeticError = DivisionByZero | NegativeExponent deriving (Eq, Show)

-- liftM2 :: Monad m => (a1 -> a2 -> r) -> m a1 -> m a2 -> m r

eval :: Expr -> Either ArithmeticError Int
eval (Const x)      = return x
eval (Add left right)  = liftM2 (+) (eval left) (eval right)
eval (Sub left right)  = liftM2 (-) (eval right) (eval left)
eval (Mul left right)  = liftM2 (*) (eval left) (eval right)
eval (Div left right)  = case eval right of
    (Right 0)   -> Left DivisionByZero
    a@(Right _) -> liftM2 div (eval left) a
    a@(Left _)  -> a
eval (Pow left right)  = case eval right of
    a@(Right e) -> if e < 0
                   then Left NegativeExponent
                   else liftM2 (^) (eval left) a
    a@(Left _) -> a

-- task2

bin :: Int -> [[Int]]
bin 0 = [[]]
bin n = bin (n - 1) >>= \set -> [0:set, 1:set]
