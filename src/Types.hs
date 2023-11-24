module Types where

import qualified Data.Map.Strict as M

type Ident = String
newtype Proposition = Prop Ident deriving (Show, Eq, Ord)

data Formula
  = Bottom
  | Top
  | P Proposition
  | Not Formula
  | And Formula Formula
  | Or Formula Formula
  | Implies Formula Formula
  deriving (Show, Eq, Ord)

type Valuation = M.Map Proposition Bool
