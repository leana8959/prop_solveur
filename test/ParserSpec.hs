module ParserSpec where

import           Test.Hspec
import           Test.QuickCheck
import           Text.Megaparsec

import           Data.Foldable         (for_)
import           Data.List             (intercalate)
import           Parser
import           Types

import           Test.Hspec.Megaparsec

validate = mapM_ (\(input, expect) -> parse pFormula "" input `shouldParse` expect)

spec :: Spec
spec = describe
  "Parser"
  $ do
    it "should parse implies"
      $ validate
        [ ("a -> b", Implies (P (Prop "a")) (P (Prop "b")))
        , ("a => b", Implies (P (Prop "a")) (P (Prop "b")))
        ]
    it "should parse and"
      $ validate
        [ ("a and b", And (P (Prop "a")) (P (Prop "b")))
        , ("a ^ b", And (P (Prop "a")) (P (Prop "b")))
        ]
    it "should parse or"
      $ validate
        [ ("a or b", Or (P (Prop "a")) (P (Prop "b")))
        , ("a v b", Or (P (Prop "a")) (P (Prop "b")))
        ]
    it "should respect precedence and parentheses"
      $ validate
        [
          ( "a or (b and c)"
          , Or (P (Prop "a")) (And (P (Prop "b")) (P (Prop "c")))
          )
        ,
          ( "a or b and c"
          , And (Or (P (Prop "a")) (P (Prop "b"))) (P (Prop "c"))
          )
        ,
          ( "a and (b or c)"
          , And (P (Prop "a")) (Or (P (Prop "b")) (P (Prop "c")))
          )
        ,
          ( "a and b or c"
          , And (P (Prop "a")) (Or (P (Prop "b")) (P (Prop "c")))
          )
        ,
          ( "(a and b) or c"
          , Or (And (P (Prop "a")) (P (Prop "b"))) (P (Prop "c"))
          )
        ]
    it "and should be associative"
      $ validate
        [
          ( "a and b and c"
          , And (And (P (Prop "a")) (P (Prop "b"))) (P (Prop "c"))
          )
        ]
    it "or should be associative"
      $ validate
        [
          ( "a or b or c"
          , Or (Or (P (Prop "a")) (P (Prop "b"))) (P (Prop "c"))
          )
        ]
    it "should pass more tests hehe"
      $ validate
        [
          ( "p v q v r and (p -> ~q) and (q -> ~r) and (r -> ~p)"
          , And
              ( And
                  ( And
                      (Or (Or (P (Prop "p")) (P (Prop "q"))) (P (Prop "r")))
                      (Implies (P (Prop "p")) (Not (P (Prop "q"))))
                  )
                  (Implies (P (Prop "q")) (Not (P (Prop "r"))))
              )
              (Implies (P (Prop "r")) (Not (P (Prop "p"))))
          )
        ]
    it "should handle linebreak"
      $ validate
        [
          ( unlines ["a", "b", "c"]
          , And (And (P (Prop "a")) (P (Prop "b"))) (P (Prop "c"))
          )
        ,
          ( unlines
              [ "p v q v r"
              , "p -> (~q)"
              , "q -> (~r)"
              , "r -> (~p)"
              , ""
              ]
          , And
              ( And
                  ( And
                      (Or (Or (P (Prop "p")) (P (Prop "q"))) (P (Prop "r")))
                      (Implies (P (Prop "p")) (Not (P (Prop "q"))))
                  )
                  (Implies (P (Prop "q")) (Not (P (Prop "r"))))
              )
              (Implies (P (Prop "r")) (Not (P (Prop "p"))))
          )
        ]
    it "should not halt where multiple empty lines are present"
      $ validate
        [
          ( unlines
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
                      (Or (Or (P (Prop "p")) (P (Prop "q"))) (P (Prop "r")))
                      (Implies (P (Prop "p")) (Not (P (Prop "q"))))
                  )
                  (Implies (P (Prop "q")) (Not (P (Prop "r"))))
              )
              (Implies (P (Prop "r")) (Not (P (Prop "p"))))
          )
        ]
