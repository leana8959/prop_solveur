module Parser (pFormula) where

import           Data.Char
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Types

type Input = String
type Parser = Parsec Void Input

sc :: Parser ()
sc = L.space hspace1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

pIdent :: Parser String
pIdent = lexeme (takeWhile1P Nothing isAlpha)

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

sepBy2, sepEndBy2 :: Parser a -> Parser b -> Parser [a]
sepBy2 p sp    = try $ (:) <$> p <* sp <*> sepBy1 p sp
sepEndBy2 p sp = try $ (:) <$> p <* sp <*> sepEndBy1 p sp

impliesSymbol, notSymbol, andSymbol, orSymbol :: Parser String
impliesSymbol = symbol "->"  <|> symbol "=>"
notSymbol     = symbol "not" <|> symbol "~"
andSymbol     = symbol "and" <|> symbol "^"
orSymbol      = symbol "or"  <|> symbol "v"

pTop, pBot, pProp :: Parser Formula
pTop  = Top      <$  symbol "top"
pBot  = Bottom   <$  symbol "bot"
pProp = P . Prop <$> pIdent

pSimple :: Parser Formula
pSimple = choice [parens pExpr, pTop, pBot, pProp]

pNot, pOr, pAnd, pImplies, pLineBreak :: Parser Formula
pNot       = (Not            <$  notSymbol <*> pSimple)          <|> pSimple
pOr        = try (foldl1 Or  <$> sepBy2 pNot orSymbol)           <|> pNot
pAnd       = try (foldl1 And <$> sepBy2 pOr andSymbol)           <|> pOr
pImplies   = try (Implies    <$> pAnd <* impliesSymbol <*> pAnd) <|> pAnd
pLineBreak = try (foldl1 And <$> sepEndBy2 pImplies eol)         <|> pImplies

pExpr :: Parser Formula
pExpr = pLineBreak

pFormula :: Parser Formula
pFormula = pExpr <* eof
