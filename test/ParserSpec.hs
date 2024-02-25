module ParserSpec where

import qualified Data.Text as T (unlines)
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec

import Parser
import Types


validate = mapM_ (\(input, expect) -> parse pFormula "" input `shouldParse` expect)

spec :: Spec
spec = describe
  "Parser"
  $ do
    it "should parse implies"
      $ validate
        [ ("a -> b", Implies (Prop "a") (Prop "b"))
        , ("a => b", Implies (Prop "a") (Prop "b"))
        ]
    it "should parse and"
      $ validate
        [ ("a and b", And (Prop "a") (Prop "b"))
        , ("a ^ b", And (Prop "a") (Prop "b"))
        ]
    it "should parse or"
      $ validate
        [ ("a or b", Or (Prop "a") (Prop "b"))
        , ("a v b", Or (Prop "a") (Prop "b"))
        ]
    it "should respect precedence and parentheses"
      $ validate
        [
          ( "a or (b and c)"
          , Or (Prop "a") (And (Prop "b") (Prop "c"))
          )
        ,
          ( "a or b and c"
          , And (Or (Prop "a") (Prop "b")) (Prop "c")
          )
        ,
          ( "a and (b or c)"
          , And (Prop "a") (Or (Prop "b") (Prop "c"))
          )
        ,
          ( "a and b or c"
          , And (Prop "a") (Or (Prop "b") (Prop "c"))
          )
        ,
          ( "(a and b) or c"
          , Or (And (Prop "a") (Prop "b")) (Prop "c")
          )
        ]
    it "and should be associative"
      $ validate
        [
          ( "a and b and c"
          , And (And (Prop "a") (Prop "b")) (Prop "c")
          )
        ]
    it "or should be associative"
      $ validate
        [
          ( "a or b or c"
          , Or (Or (Prop "a") (Prop "b")) (Prop "c")
          )
        ]
    it "should pass more tests hehe"
      $ validate
        [
          ( "p v q v r and (p -> ~q) and (q -> ~r) and (r -> ~p)"
          , And
              ( And
                  ( And
                      (Or (Or (Prop "p") (Prop "q")) (Prop "r"))
                      (Implies (Prop "p") (Not (Prop "q")))
                  )
                  (Implies (Prop "q") (Not (Prop "r")))
              )
              (Implies (Prop "r") (Not (Prop "p")))
          )
        ]
    it "should handle linebreak"
      $ validate
        [
          ( T.unlines ["a", "b", "c"]
          , And (And (Prop "a") (Prop "b")) (Prop "c")
          )
        ,
          ( T.unlines
              [ "p v q v r"
              , "p -> (~q)"
              , "q -> (~r)"
              , "r -> (~p)"
              , ""
              ]
          , And
              ( And
                  ( And
                      (Or (Or (Prop "p") (Prop "q")) (Prop "r"))
                      (Implies (Prop "p") (Not (Prop "q")))
                  )
                  (Implies (Prop "q") (Not (Prop "r")))
              )
              (Implies (Prop "r") (Not (Prop "p")))
          )
        ]
    it "should not halt where multiple empty lines are present"
      $ validate
        [
          ( T.unlines
              [ "p v q v r"
              , ""
              , "p -> (~q)"
              , ""
              , ""
              , ""
              , "q -> (~r)"
              , "r -> (~p)"
              , ""
              ]
          , And
              ( And
                  ( And
                      (Or (Or (Prop "p") (Prop "q")) (Prop "r"))
                      (Implies (Prop "p") (Not (Prop "q")))
                  )
                  (Implies (Prop "q") (Not (Prop "r")))
              )
              (Implies (Prop "r") (Not (Prop "p")))
          )
        ]
