module Types
  ( Exp (..)
  , Parser (..)
  , Tape
  , Program
  )
where

import Control.Applicative

data Exp
  = Plus
  | Minus
  | MoveLeft
  | MoveRight
  | Loop [Exp]
  | Input
  | Output
  deriving (Show, Eq)

newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}

instance Functor Parser where
  fmap f (Parser p) = Parser $ \input -> do
    (x, input') <- p input
    Just (f x, input')

instance Applicative Parser where
  pure x = Parser $ \input -> Just (x, input)
  (Parser p1) <*> (Parser p2) = Parser $ \input -> do
    (f, input') <- p1 input
    (x, input'') <- p2 input'
    Just (f x, input'')

instance Alternative Parser where
  empty = Parser $ const Nothing
  (Parser p1) <|> (Parser p2) = Parser $ \input -> p1 input <|> p2 input

type Tape = ([Int], Int, [Int])

type Program = [Exp]
