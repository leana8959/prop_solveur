module Parser where

import           Data.Char                  (isAlphaNum)
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
pIdent = lexeme (takeWhile1P Nothing isAlphaNum)

parens :: Lexer a -> Lexer a
parens = try . between (symbol "(") (symbol ")")

pBottom, pTop, pP, pNot, pAnd, pOr, pImplies :: Lexer Formula
pBottom  = Bottom <$ symbol "bot"
pTop     = Top <$ symbol "top"
pP       = P . Prop <$> pIdent
pNot     = parens $ Not <$ symbol "not" <*> pFormula
pAnd     = parens $ And <$> pFormula <* symbol "and" <*> pFormula
pOr      = parens $ Or <$> pFormula <* symbol "or" <*> pFormula
pImplies = parens $ Implies <$> pFormula <* symbol "->" <*> pFormula

pFormula :: Lexer Formula
pFormula = choice
  [ pImplies
  , pNot
  , pAnd
  , pOr
  , pP
  , pBottom
  , pTop
  ]
