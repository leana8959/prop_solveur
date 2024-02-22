module Types where

import qualified Data.Map.Strict as M

import Data.Text (Text)

type Ident = Text
type Valuation = M.Map Ident Bool

data Formula
  = Bottom
  | Top
  | Prop Ident
  | Not Formula
  | And Formula Formula
  | Or Formula Formula
  | Implies Formula Formula
  deriving (Show, Eq)

