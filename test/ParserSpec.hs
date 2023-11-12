module ParserSpec where

import Test.Hspec
import Test.QuickCheck
import Text.Megaparsec

import Data.Foldable (for_)
import Parser
import Types

run :: String -> Maybe Formula
run input =
  let output = runParser pFormula "" input
  in  case output of
        Left err -> Nothing
        Right res -> Just res

validate xs = for_ xs (uncurry shouldBe)

spec :: Spec
spec = describe
  "Parser"
  $ do
    it "should parse implies"
      $ validate
        [ (run "a -> b", Just (Implies (P (Prop "a")) (P (Prop "b"))))
        , (run "a => b", Just (Implies (P (Prop "a")) (P (Prop "b"))))
        ]
    it "should parse and"
      $ validate
        [ (run "a and b", Just (And (P (Prop "a")) (P (Prop "b"))))
        , (run "a ^ b", Just (And (P (Prop "a")) (P (Prop "b"))))
        ]
    it "should parse or"
      $ validate
        [ (run "a or b", Just (Or (P (Prop "a")) (P (Prop "b"))))
        , (run "a v b", Just (Or (P (Prop "a")) (P (Prop "b"))))
        ]
    it "should respect precedence and parentheses"
      $ validate
        [
          ( run "a or (b and c)"
          , Just (Or (P (Prop "a")) (And (P (Prop "b")) (P (Prop "c"))))
          )
        ,
          ( run "a or b and c"
          , Just (And (Or (P (Prop "a")) (P (Prop "b"))) (P (Prop "c")))
          )
        ,
          ( run "a and (b or c)"
          , Just (And (P (Prop "a")) (Or (P (Prop "b")) (P (Prop "c"))))
          )
        ,
          ( run "a and b or c"
          , Just (And (P (Prop "a")) (Or (P (Prop "b")) (P (Prop "c"))))
          )
        ,
          ( run "(a and b) or c"
          , Just (Or (And (P (Prop "a")) (P (Prop "b"))) (P (Prop "c")))
          )
        ]
