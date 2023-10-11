module Main where

import Control.Arrow (Arrow (second))
import Data.List
import qualified Data.Set as S

import Debug.Trace (traceShowId)
import Prelude hiding (negate)

type Ident = String
newtype Proposition = Prop Ident deriving (Show, Eq, Ord)

data Formule
  = Bottom
  | Top
  | P Proposition
  | Non Formule
  | Et Formule Formule
  | Ou Formule Formule
  | Implique Formule Formule
  deriving (Show, Eq, Ord)

hauteur :: Formule -> Int
hauteur f = case f of
  Bottom -> 0
  Top -> 0
  P _ -> 0
  Non f -> 1 + hauteur f
  Et f1 f2 -> 1 + max (hauteur f1) (hauteur f2)
  Ou f1 f2 -> 1 + max (hauteur f1) (hauteur f2)
  Implique f1 f2 -> 1 + max (hauteur f1) (hauteur f2)

nombreDeOu :: Formule -> Int
nombreDeOu f = case f of
  Bottom -> 0
  Top -> 0
  P _ -> 0
  Non f -> hauteur f
  Et f1 f2 -> max (hauteur f1) (hauteur f2)
  Ou f1 f2 -> 1 + max (hauteur f1) (hauteur f2)
  Implique f1 f2 -> max (hauteur f1) (hauteur f2)

sousFormule :: Formule -> S.Set Formule
sousFormule f = case f of
  Bottom -> S.singleton Bottom
  Top -> S.singleton Top
  P p -> S.singleton (P p)
  Non f ->
    let this = S.singleton (Non f)
        f' = sousFormule f
    in  S.union this (sousFormule f)
  Et f1 f2 ->
    let this = S.singleton (Et f1 f2)
        f1' = sousFormule f1
        f2' = sousFormule f2
    in  S.union this (S.union f1' f2')
  Ou f1 f2 ->
    let this = S.singleton (Ou f1 f2)
        f1' = sousFormule f1
        f2' = sousFormule f2
    in  S.union this (S.union f1' f2')
  Implique f1 f2 ->
    let this = S.singleton (Implique f1 f2)
        f1' = sousFormule f1
        f2' = sousFormule f2
    in  S.union this (S.union f1' f2')

sousFormuleStricte :: Formule -> S.Set Formule
sousFormuleStricte f = case f of
  Bottom -> S.empty
  Top -> S.empty
  P _ -> S.empty
  Non f ->
    let this = S.singleton f
        f' = sousFormuleStricte f
    in  S.union this f'
  Et f1 f2 ->
    let this = S.fromList [f1, f2]
        f1' = sousFormuleStricte f1
        f2' = sousFormuleStricte f2
    in  S.union this (S.union f1' f2')
  Ou f1 f2 ->
    let this = S.fromList [f1, f2]
        f1' = sousFormuleStricte f1
        f2' = sousFormuleStricte f2
    in  S.union this (S.union f1' f2')
  Implique f1 f2 ->
    let this = S.fromList [f1, f2]
        f1' = sousFormuleStricte f1
        f2' = sousFormuleStricte f2
    in  S.union this (S.union f1' f2')

type Valuation = [(Proposition, Bool)]

gen :: [Proposition] -> [Valuation]
gen ps =
  let l = length ps
      perm 0 = []
      perm 1 = [[True], [False]]
      perm n = [p : t | p <- [True, False], t <- perm (n - 1)]
  in  map (zip ps) (perm l)

findProp :: Formule -> [Proposition]
findProp f = case f of
  (P p) -> [p]
  Non f -> findProp f
  Et f1 f2 -> findProp f1 ++ findProp f2
  Ou f1 f2 -> findProp f1 ++ findProp f2
  Implique f1 f2 -> findProp f1 ++ findProp f2
  _ -> []

findPropNub :: Formule -> [Proposition]
findPropNub = nub . findProp

eval :: Formule -> Valuation -> Bool
eval f vs = case f of
  Top -> True
  Bottom -> False
  P p -> any (\(prop, v) -> prop == p && v) vs
  Non f -> not $ eval f vs
  Et f1 f2 -> eval f1 vs && eval f2 vs
  Ou f1 f2 -> eval f1 vs || eval f2 vs
  Implique f1 f2 -> not (eval f1 vs) || eval f2 vs

solve :: Formule -> [Valuation]
solve f =
  let props = gen $ findPropNub f
      res = map (eval f) props
      ts = map fst . filter snd $ zip props res
  in  ts

p = P $ Prop "p"
q = P $ Prop "q"

showExamples :: [Formule] -> [IO ()]
showExamples fs = putStrLn . uncurry showFormule <$> zip [0 ..] fs
  where
    showFormule i f = unlines ["exemple nº" ++ show i, show f, show $ solve f]

main =
  sequence $
    showExamples
      [ Et Top p,
        Ou Top p,
        Non p,
        Et p q,
        Ou p q,
        Et p p
      ]
