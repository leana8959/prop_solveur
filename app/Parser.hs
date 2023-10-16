module Parser (pFormula) where

import           Data.Char                  (isAlpha)
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Types

type Input = String
type Lexer = Parsec Void Input

sc :: Lexer ()
sc = L.space space1 empty empty

lexeme :: Lexer a -> Lexer a
lexeme = L.lexeme sc

symbol :: String -> Lexer String
symbol = L.symbol sc

pIdent :: Lexer String
pIdent = lexeme (takeWhile1P Nothing isAlpha)

parens :: Lexer a -> Lexer a
parens = between (symbol "(") (symbol ")")

sepBy2 :: Lexer a -> Lexer b -> Lexer [a]
sepBy2 p sp = try $ do
  one  <- p
  sp
  rest <- sepBy1 p sp
  return $ one : rest

pBottom, pTop, pP, pNot, pAnd, pOr, pImplies :: Lexer Formula
pBottom  = Bottom         <$  symbol "bot"
pTop     = Top            <$  symbol "top"
pP       = P . Prop       <$> pIdent
pNot     = Not            <$  symbol "not" <*> pTermOrComposite
pAnd     = foldl1 And     <$> sepBy2 pTermOrComposite (symbol "and")
pOr      = foldl1 Or      <$> sepBy2 pTermOrComposite (symbol "or")
pImplies = foldl1 Implies <$> sepBy2 pTermOrComposite (symbol "->")

pTerm, pComposite, pTermOrComposite :: Lexer Formula
pTerm = choice
  [ pBottom
  , pTop
  , pP
  ]
pComposite = choice
  [ pImplies
  , pAnd
  , pOr
  , pNot
  ]
pTermOrComposite = try $ parens pComposite <|> pTerm

pFormula :: Lexer Formula
pFormula = choice
  [ pComposite
  , pTerm
  ] <* eof
