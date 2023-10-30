module Main (main) where

import Evaluator
import Parser
import Types

main :: IO ()
main = do
  putStrLn "Welcome to the BrainFuck interpreter!"
  input <- getLine
  eval . errorHandler . runParser parseBrainfuck $ input

---TODO: this sucks!
errorHandler :: Maybe (Program, String) -> Program
errorHandler je = case je of
  Nothing -> error "The expression is not well formed"
  (Just e) ->
    if snd e /= ""
      then error "The expression is not well formed"
      else fst e
