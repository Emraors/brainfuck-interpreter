module Evaluator
  ( eval,
    evalExpr,
  )
where

import Control.Monad.Free
import Control.Monad.State
import Types

evalExpr :: Exp -> StateT Tape IO ()
evalExpr Plus = modify (\(l, c, r) -> (l, c + 1, r))
evalExpr Minus = modify (\(l, c, r) -> (l, c - 1, r))
evalExpr MoveLeft = modify (\(l, c, r) -> (tail l, head l, c : r))
evalExpr MoveRight = modify (\(l, c, r) -> (c : l, head r, tail r))
evalExpr (Loop prog) = get >>= \(_, c, _) -> when (c /= 0) $ evalProgram prog >> evalExpr (Loop prog)
evalExpr Input = liftIO getChar >>= \c -> modify (\(l, _, r) -> (l, fromEnum c, r))
evalExpr Output = get >>= \(_, c, _) -> liftIO . putChar . toEnum $ c

evalProgram :: Program -> StateT Tape IO ()
evalProgram = mapM_ evalExpr

eval :: Program -> IO ()
eval prog = evalStateT (evalProgram prog) ([], 0, repeat 0)
