{-# LANGUAGE LambdaCase #-}

module Parser
  ( someFunc,
    parseBrainfuck,
    parseChar,
    parseInput,
    parseLoop,
    parseLoopToken,
    parseMinus,
    parsePlus,
    parseMoveLeft,
    parseMoveRight,
    parseOutput,
    parseToken,
    runParser,
  )
where

import Control.Applicative
import Types

someFunc :: IO ()
someFunc = putStrLn "someFunc"

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser $ \input -> case input of
  [] -> Nothing
  (x : xs) -> if p x then Just (x, xs) else Nothing

parseChar :: Char -> Parser Char
parseChar c = satisfy (== c)

parsePlus :: Parser Exp
parsePlus = Plus <$ parseChar '+'

parseMinus :: Parser Exp
parseMinus = Minus <$ parseChar '-'

parseMoveLeft :: Parser Exp
parseMoveLeft = MoveLeft <$ parseChar '<'

parseMoveRight :: Parser Exp
parseMoveRight = MoveRight <$ parseChar '>'

parseInput :: Parser Exp
parseInput = Input <$ parseChar ','

parseOutput :: Parser Exp
parseOutput = Output <$ parseChar '.'

parseLoop :: Parser Program
parseLoop = parseChar '[' *> many parseToken <* parseChar ']'

parseLoopToken :: Parser Exp
parseLoopToken = Loop <$> parseLoop

parseToken :: Parser Exp
parseToken =
  parsePlus
    <|> parseMinus
    <|> parseMoveLeft
    <|> parseMoveRight
    <|> parseInput
    <|> parseOutput
    <|> parseLoopToken

parseBrainfuck :: Parser Program
parseBrainfuck = many parseToken
