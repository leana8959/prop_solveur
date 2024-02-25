module Parser (pFormula) where

import Data.Char
import Data.Text (Text)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

import Types

type Input = Text
type Parser = Parsec Void Input

sc :: Parser ()
sc = skipMany hspace1

lexeme :: Parser a -> Parser a
lexeme p = p <* sc

symbol :: Text -> Parser Text
symbol s = string s <* sc

pIdent :: Parser Text
pIdent = lexeme (takeWhile1P (Just "variable name") isAlpha)

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

sepBy2, sepEndBy2 :: Parser a -> Parser b -> Parser [a]
sepBy2 p sp    = try $ (:) <$> p <* sp <*> p `sepBy1` sp
sepEndBy2 p sp = try $ (:) <$> p <* sp <*> p `sepEndBy1` sp

impliesSymbol, notSymbol, andSymbol, orSymbol :: Parser Text
impliesSymbol = symbol "->"  <|> symbol "=>"
notSymbol     = symbol "not" <|> symbol "~"
andSymbol     = symbol "and" <|> symbol "^"
orSymbol      = symbol "or"  <|> symbol "v"

pTop, pBot, pProp :: Parser Formula
pTop  = Top    <$  symbol "top"
pBot  = Bottom <$  symbol "bot"
pProp = Prop   <$> pIdent

pSimple :: Parser Formula
pSimple = choice [parens pExpr, pTop, pBot, pProp]

pNot, pOr, pAnd, pImplies, pExpr :: Parser Formula
pNot     = (Not            <$  notSymbol <*> pSimple)          <|> pSimple
pOr      = try (foldl1 Or  <$> pNot `sepBy2` orSymbol)         <|> pNot
pAnd     = try (foldl1 And <$> pOr `sepBy2` andSymbol)         <|> pOr
pImplies = try (Implies    <$> pAnd <* impliesSymbol <*> pAnd) <|> pAnd
pExpr    = try (foldl1 And <$> pImplies `sepEndBy2` some eol)  <|> pImplies

pFormula :: Parser Formula
pFormula = between blankLines blankLines pExpr <* eof
  where blankLines = skipMany eol
