module ParserSpec where

import           Test.Hspec
import           Test.QuickCheck
import           Text.Megaparsec

import           Data.Foldable   (for_)
import           Data.List       (intercalate)
import           Parser
import           Types

run :: String -> Maybe Formula
run input =
  let output = runParser pFormula "" input
  in  case output of
        Left err  -> Nothing
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
    it "and should be associative"
      $ validate
        [
          ( run "a and b and c"
          , Just (And (And (P (Prop "a")) (P (Prop "b"))) (P (Prop "c")))
          )
        ]
    it "or should be associative"
      $ validate
        [
          ( run "a or b or c"
          , Just (Or (Or (P (Prop "a")) (P (Prop "b"))) (P (Prop "c")))
          )
        ]
    it "should pass more tests hehe"
      $ validate
        [
          ( run "p v q v r and (p -> ~q) and (q -> ~r) and (r -> ~p)"
          , Just
              ( And
                  ( And
                      ( And
                          (Or (Or (P (Prop "p")) (P (Prop "q"))) (P (Prop "r")))
                          (Implies (P (Prop "p")) (Not (P (Prop "q"))))
                      )
                      (Implies (P (Prop "q")) (Not (P (Prop "r"))))
                  )
                  (Implies (P (Prop "r")) (Not (P (Prop "p"))))
              )
          )
        ]
    it "should handle linebreak"
      $ validate
        [
          ( run
              $ intercalate
                "\n"
                ["a", "b", "c"]
          , Just (And (And (P (Prop "a")) (P (Prop "b"))) (P (Prop "c")))
          )
        ,
          ( run
              $ intercalate
                "\n"
                [ "p v q v r"
                , "p -> (~q)"
                , "q -> (~r)"
                , "r -> (~p)"
                , ""
                ]
          , Just
              ( And
                  ( And
                      ( And
                          (Or (Or (P (Prop "p")) (P (Prop "q"))) (P (Prop "r")))
                          (Implies (P (Prop "p")) (Not (P (Prop "q"))))
                      )
                      (Implies (P (Prop "q")) (Not (P (Prop "r"))))
                  )
                  (Implies (P (Prop "r")) (Not (P (Prop "p"))))
              )
          )
        ]
