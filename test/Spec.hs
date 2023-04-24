import Control.Monad.State
import Evaluator
import Parser
import System.IO.Silently
import Test.Hspec
import Test.QuickCheck
import Types
import Test.Main
import Data.ByteString.Char8 (pack)


main :: IO ()
main = hspec $ do
  describe "parseChar" $ do
    it "should works as expected" $ do
      property $ \c -> runParser (parseChar c) [c] == Just (c, "")

    it "should return nothing" $ do
      runParser (parseChar 'a') "bc" `shouldBe` Nothing

    it "should return nothing" $ do
      runParser (parseChar 'a') "bac" `shouldBe` Nothing

  describe "parse simple tokens" $ do
    it "should parse all correct token" $ do
      runParser parsePlus "+" `shouldBe` Just (Plus, "")

    it "should parse Minus" $ do
      runParser parseMinus "-" `shouldBe` Just (Minus, "")

    it "should parse Output" $ do
      runParser parseOutput "." `shouldBe` Just (Output, "")

    it "should parse Input" $ do
      runParser parseInput "," `shouldBe` Just (Input, "")

    it "should parse MoveRight" $ do
      runParser parseMoveRight ">" `shouldBe` Just (MoveRight, "")

    it "should parse MoveLeft" $ do
      runParser parseMoveLeft "<" `shouldBe` Just (MoveLeft, "")

  describe "tokenToString" $ do
    it "should convert Plus" $ do
      tokenToString Plus `shouldBe` "+"

    it "should convert Minus" $ do
      tokenToString Minus `shouldBe` "-"

    it "should convert Output" $ do
      tokenToString Output `shouldBe` "."

    it "should convert Input" $ do
      tokenToString Input `shouldBe` ","

    it "should convert MoveRight" $ do
      tokenToString MoveRight `shouldBe` ">"

    it "should convert MoveLeft" $ do
      tokenToString MoveLeft `shouldBe` "<"

    it "should convert Loop" $ do
      tokenToString (Loop [Plus, Minus]) `shouldBe` "[+-]"

  describe "parseBrainfuck" $ do
    it "should parse a valid string" $ do
      property prop_parseIsIdempotent

    it "should fail for unbalanced parenthesis" $ do
      runParser parseBrainfuck "++[" `shouldBe` Just ([Plus, Plus], "[")
      runParser parseBrainfuck "+]" `shouldBe` Just ([Plus], "]")

    it "should fail for wrong character" $ do
      runParser parseBrainfuck "+a..." `shouldBe` Just ([Plus], "a...")

  describe "EvalExpr" $ do
    it "should increment the current cell when eval Plus" $ do
      captured <- capture $ runStateT (evalExpr Plus) ([], 0, [])
      let tape = snd . snd $ captured
      tape `shouldBe` ([], 1, [])

    it "should decrement the current cell when eval Minus" $ do
      captured <- capture $ runStateT (evalExpr Minus) ([], 1, [])
      let tape = snd . snd $ captured
      tape `shouldBe` ([], 0, [])

    it "should move the current cell pointer one cell to the left when eval MoveLeft" $ do
      captured <- capture $ runStateT (evalExpr MoveLeft) ([0], 1, [])
      let tape = snd . snd $ captured
      tape `shouldBe` ([], 0, [1])

    it "should move the current cell pointer one cell to the right when eval MoveRight" $ do
      captured <- capture $ runStateT (evalExpr MoveRight) ([], 0, [1])
      let tape = snd . snd $ captured
      tape `shouldBe` ([0], 1, [])

    it "should set the current cell to the ASCII value of the input character when eval Input" $ do
      let inputStr = "A" -- input string
      let expectedTape = ([], 65, []) -- expected tape after executing Input
      let out = runStateT (evalExpr Input) ([], 0, [])
      captured <- capture $ withStdin (pack inputStr) out
      let tape = snd . snd $ captured
      tape `shouldBe` expectedTape

  describe "Eval" $ do
    it "should return hello world" $ do
      captured <- capture_ . eval $ helloWorld
      captured `shouldBe` "Hello World!\n"

    it "should return fibonacci" $ do
      capturered <- capture_ . eval $ fibonacci
      capturered `shouldBe` "1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89"

--------------------------------------------------------------------------------
---Utilities--------------------------------------------------------------------
--------------------------------------------------------------------------------

tokenToString :: Exp -> String
tokenToString Plus = "+"
tokenToString Minus = "-"
tokenToString Output = "."
tokenToString Input = ","
tokenToString MoveRight = ">"
tokenToString MoveLeft = "<"
tokenToString (Loop ts) = "[" ++ concatMap tokenToString ts ++ "]"

generateSimpleToken :: Gen Exp
generateSimpleToken =
  oneof
    [ pure Plus,
      pure Minus,
      pure MoveLeft,
      pure MoveRight,
      pure Input,
      pure Output
    ]

arbToken :: Int -> Gen Exp
arbToken 0 = generateSimpleToken
arbToken n =
  oneof
    [ generateSimpleToken,
      Loop <$> listOf (arbToken $ n `div` 2)
    ]

generateStringToken :: Int -> Gen String
generateStringToken n = do
  t <- arbToken n
  return $ tokenToString t

prop_parseIsIdempotent :: Property
prop_parseIsIdempotent = forAll (generateStringToken 10) $
  \t ->
    (concatMap tokenToString . fst <$> runParser parseBrainfuck t)
      === Just t

helloWorld :: Program
helloWorld = [Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Loop [MoveRight, Plus, Plus, Plus, Plus, Loop [MoveRight, Plus, Plus, MoveRight, Plus, Plus, Plus, MoveRight, Plus, Plus, Plus, MoveRight, Plus, MoveLeft, MoveLeft, MoveLeft, MoveLeft, Minus], MoveRight, Plus, MoveRight, Plus, MoveRight, Minus, MoveRight, MoveRight, Plus, Loop [MoveLeft], MoveLeft, Minus], MoveRight, MoveRight, Output, MoveRight, Minus, Minus, Minus, Output, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Output, Output, Plus, Plus, Plus, Output, MoveRight, MoveRight, Output, MoveLeft, Minus, Output, MoveLeft, Output, Plus, Plus, Plus, Output, Minus, Minus, Minus, Minus, Minus, Minus, Output, Minus, Minus, Minus, Minus, Minus, Minus, Minus, Minus, Output, MoveRight, MoveRight, Plus, Output, MoveRight, Plus, Plus, Output]

fibonacci :: Program
fibonacci = [Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, MoveRight, Plus, MoveRight, MoveRight, MoveRight, MoveRight, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, MoveRight, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, MoveLeft, MoveLeft, MoveLeft, MoveLeft, MoveLeft, MoveLeft, Loop [MoveRight, Loop [MoveRight, MoveRight, MoveRight, MoveRight, MoveRight, MoveRight, Plus, MoveRight, Plus, MoveLeft, MoveLeft, MoveLeft, MoveLeft, MoveLeft, MoveLeft, MoveLeft, Minus], MoveRight, MoveRight, MoveRight, MoveRight, MoveRight, MoveRight, MoveRight, Loop [MoveLeft, MoveLeft, MoveLeft, MoveLeft, MoveLeft, MoveLeft, MoveLeft, Plus, MoveRight, MoveRight, MoveRight, MoveRight, MoveRight, MoveRight, MoveRight, Minus], MoveLeft, Loop [MoveRight, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Loop [Minus, MoveLeft, Minus, Loop [MoveRight, MoveRight, Plus, MoveRight, Plus, MoveLeft, MoveLeft, MoveLeft, Minus], MoveRight, MoveRight, MoveRight, Loop [MoveLeft, MoveLeft, MoveLeft, Plus, MoveRight, MoveRight, MoveRight, Minus], Plus, MoveLeft, Loop [MoveRight, Loop [Minus], MoveLeft, Loop [Minus]], MoveRight, Loop [MoveLeft, MoveLeft, Loop [MoveRight, MoveRight, MoveRight, Plus, MoveLeft, MoveLeft, MoveLeft, Minus], MoveRight, MoveRight, Loop [Minus]], MoveLeft, MoveLeft], MoveRight, MoveRight, MoveRight, Loop [MoveRight, MoveRight, Plus, MoveRight, Plus, MoveLeft, MoveLeft, MoveLeft, Minus], MoveRight, MoveRight, MoveRight, Loop [MoveLeft, MoveLeft, MoveLeft, Plus, MoveRight, MoveRight, MoveRight, Minus], Plus, MoveLeft, Loop [MoveRight, Loop [Minus], MoveLeft, Loop [Minus]], MoveRight, Loop [MoveLeft, MoveLeft, Plus, MoveRight, MoveRight, Loop [Minus]], MoveLeft, MoveLeft, MoveLeft, MoveLeft, MoveLeft, MoveLeft, MoveLeft], MoveRight, MoveRight, MoveRight, MoveRight, MoveRight, Loop [Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Output, Loop [Minus]], Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, MoveLeft, Loop [Minus, MoveRight, Minus, MoveLeft], MoveRight, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Plus, Output, Loop [Minus], MoveLeft, MoveLeft, MoveLeft, MoveLeft, MoveLeft, MoveLeft, MoveLeft, MoveLeft, MoveLeft, MoveLeft, MoveLeft, MoveLeft, Loop [MoveRight, MoveRight, MoveRight, Plus, MoveRight, Plus, MoveLeft, MoveLeft, MoveLeft, MoveLeft, Minus], MoveRight, MoveRight, MoveRight, MoveRight, Loop [MoveLeft, MoveLeft, MoveLeft, MoveLeft, Plus, MoveRight, MoveRight, MoveRight, MoveRight, Minus], MoveLeft, Minus, Loop [MoveRight, MoveRight, Output, MoveRight, Output, MoveLeft, MoveLeft, MoveLeft, Loop [Minus]], MoveLeft, MoveLeft, Loop [MoveRight, MoveRight, Plus, MoveRight, Plus, MoveLeft, MoveLeft, MoveLeft, Minus], MoveRight, MoveRight, MoveRight, Loop [MoveLeft, MoveLeft, MoveLeft, Plus, MoveRight, MoveRight, MoveRight, Minus], MoveLeft, MoveLeft, Loop [MoveLeft, Plus, MoveRight, Minus], MoveRight, Loop [MoveLeft, Plus, MoveRight, Minus], MoveLeft, MoveLeft, MoveLeft, Minus]]
