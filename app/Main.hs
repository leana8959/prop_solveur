module Main where

import Control.Arrow (Arrow (second))
import Data.Function (on)
import Data.List
import qualified Data.Map.Strict as M
import qualified Data.Set as S

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

main :: IO ()
main = putStrLn "Hello, Haskell!"
