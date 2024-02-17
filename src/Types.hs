module Types where

import qualified Data.Map.Strict as M

type Ident = String

data Formula
  = Bottom
  | Top
  | Prop Ident
  | Not Formula
  | And Formula Formula
  | Or Formula Formula
  | Implies Formula Formula
  deriving (Show, Eq)

type Valuation = M.Map Ident Bool
