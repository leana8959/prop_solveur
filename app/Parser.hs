module Parser (pFormula) where

import           Data.Char
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Types

type Input = String
type Lexer = Parsec Void Input

sc :: Lexer ()
sc = L.space hspace1 empty empty

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

arrowSymbol = symbol "->"  <|> symbol "=>"
notSymbol   = symbol "not" <|> symbol "~"
andSymbol   = symbol "and" <|> symbol "^"
orSymbol    = symbol "or"  <|> symbol "v"

pBottom, pTop, pP, pNot, pAnd, pOr, pImplies :: Lexer Formula
pBottom  = Bottom         <$  symbol "bot"
pTop     = Top            <$  symbol "top"
pP       = P . Prop       <$> pIdent
pNot     = Not            <$  notSymbol <*> pTermOrComposite
pAnd     = foldl1 And     <$> sepBy2 pTermOrComposite andSymbol
pOr      = foldl1 Or      <$> sepBy2 pTermOrComposite orSymbol
pImplies = foldl1 Implies <$> sepBy2 pTermOrComposite arrowSymbol

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
pTermOrComposite = parens pComposite <|> pTerm

pFormula :: Lexer Formula
pFormula = (parens p <|> p) <* eof
  where
    p = choice
      [ pComposite
      , pTerm
      ]
