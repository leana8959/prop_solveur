module Solver where

import Data.Bifunctor (Bifunctor(first))
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T

import Types (Formula(..), Ident, Valuation)

-- | Générer toutes les valuations possible (ensemble `Val`)
gen :: [Ident] -> [Valuation]
gen ps =
  let l       = length ps
      bools 0 = [[]]
      bools n = [p : t | p <- [True, False], t <- bools (n - 1)]
  in  map (M.fromList . zip ps) (bools l)

-- | Trouver toutes les propositions
findProp :: Formula -> [Ident]
findProp =
  let go :: Formula -> S.Set Ident
      go (Prop p)           = S.singleton p
      go (Not f)         = go f
      go (And f1 f2)     = S.union (go f1) (go f2)
      go (Or f1 f2)      = S.union (go f1) (go f2)
      go (Implies f1 f2) = S.union (go f1) (go f2)
      go _               = S.empty
  in S.toList . go

-- | Evaluer une formule étant donné une valuation
eval :: Formula -> Valuation -> Bool
eval f vs = case f of
  Top           -> True
  Bottom        -> False
  Prop p           -> vs M.! p
  Not f         -> not (eval f vs)
  And f1 f2     -> eval f1 vs && eval f2 vs
  Or f1 f2      -> eval f1 vs || eval f2 vs
  Implies f1 f2 -> not (eval f1 vs) || eval f2 vs

-- | Trouver toutes les valuations qui satisfait une formule
solve :: Formula -> [Valuation]
solve f =
  let vals = gen $ findProp f
      res  = map (eval f) vals
      ts   = map fst . filter snd $ zip vals res
  in  ts

showSolution :: Valuation -> Int -> String
showSolution v i =
  unlines
  $ ("solution nº" ++ show i)
    : map ((\(p, value) -> p ++ ": " ++ show value) . first T.unpack) (M.toList v)

showSolutions :: [Valuation] -> String
showSolutions vs = unlines $ uncurry showSolution <$> zip vs [1..]
