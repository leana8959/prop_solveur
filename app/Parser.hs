module Parser (pFormula) where

import Data.Char
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Types

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

sepBy2 :: Parser a -> Parser b -> Parser [a]
sepBy2 p sp = try $ (:) <$> p <* sp <*> sepBy1 p sp

sepEndBy2 :: Parser a -> Parser b -> Parser [a]
sepEndBy2 p sp = try $ (:) <$> p <* sp <*> sepEndBy1 p sp

impliesSymbol = symbol "->" <|> symbol "=>"
notSymbol = symbol "not" <|> symbol "~"
andSymbol = symbol "and" <|> symbol "^"
orSymbol = symbol "or" <|> symbol "v"

pTop, pBot :: Parser Formula
pTop = Top <$ symbol "top"
pBot = Bottom <$ symbol "bot"

pProp :: Parser Formula
pProp = P . Prop <$> pIdent

pSimple :: Parser Formula
pSimple = parens pExpr <|> pTop <|> pBot <|> pProp

pNot :: Parser Formula
pNot = try (Not <$ notSymbol <*> pSimple) <|> pSimple

pOr :: Parser Formula
pOr = try (foldl1 Or <$> sepBy2 pNot orSymbol) <|> pNot

pAnd :: Parser Formula
pAnd = try (foldl1 And <$> sepBy2 pOr andSymbol) <|> pOr

pImplies :: Parser Formula
pImplies = try (Implies <$> pAnd <* impliesSymbol <*> pAnd) <|> pAnd

pLineBreak :: Parser Formula
pLineBreak = try (foldl1 And <$> sepEndBy2 pImplies eol) <|> pImplies

pExpr :: Parser Formula
pExpr = pLineBreak

pFormula :: Parser Formula
pFormula = pExpr <* eof
