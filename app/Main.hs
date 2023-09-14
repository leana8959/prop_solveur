module Main where

import Data.List

-- FIXME: What should I put here?
newtype Proposition = Prop String deriving (Show, Eq)

data Formule
  = Bottom
  | Top
  | P Proposition
  | Non Formule
  | Et Formule Formule
  | Ou Formule Formule
  | Implique Formule Formule
  deriving (Show, Eq)

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

sousFormule :: Formule -> [Formule]
sousFormule f = case f of
  Bottom -> [Bottom]
  Top -> [Top]
  P p -> [P p]
  Non f -> Non f : sousFormule f
  Et f1 f2 -> [Et f1 f2] ++ sousFormule f1 ++ sousFormule f2
  Ou f1 f2 -> [Ou f1 f2] ++ sousFormule f1 ++ sousFormule f2
  Implique f1 f2 -> [Implique f1 f2] ++ sousFormule f1 ++ sousFormule f2

sousFormuleStricte :: Formule -> [Formule]
sousFormuleStricte f = case f of
  Bottom -> []
  Top -> []
  P _ -> []
  Non f -> f : sousFormule f
  Et f1 f2 -> [f1, f2] ++ sousFormule f1 ++ sousFormule f2
  Ou f1 f2 -> [f1, f2] ++ sousFormule f1 ++ sousFormule f2
  Implique f1 f2 -> [f1, f2] ++ sousFormule f1 ++ sousFormule f2

simp :: Formule -> Formule
simp Bottom = Bottom
simp Top = Top
simp (P p) = P p
simp (Non f) = case simp f of
  Bottom -> Top
  Top -> Bottom
  f' -> Non f'
simp (Et f1 f2) = case (simp f1, simp f2) of
  (Bottom, Bottom) -> Bottom
  (Bottom, Top) -> Bottom
  (Top, Bottom) -> Bottom
  (Top, Top) -> Top
  (f1', f2') -> Et f1' f2'
simp (Ou f1 f2) = case (simp f1, simp f2) of
  (Bottom, Bottom) -> Bottom
  (Bottom, Top) -> Top
  (Top, Bottom) -> Top
  (Top, Top) -> Top
  (f1', f2') -> Et f1' f2'
simp (Implique f1 f2) = case (simp f1, simp f2) of
  (Bottom, Bottom) -> Top
  (Bottom, Top) -> Top
  (Top, Bottom) -> Bottom
  (Top, Top) -> Top
  (f1', f2') -> Et f1' f2'

solve :: Formule -> Maybe [(Proposition, Formule)]
solve f = case simp f of
  Bottom -> Nothing
  Top -> Just []
  P p -> Just [(p, Bottom), (p, Top)]
  Non f -> filter ((== Bottom) . snd) <$> solve f
  Et f1 f2 -> do
    x <- filter ((== Top) . snd) <$> solve f1
    y <- filter ((== Top) . snd) <$> solve f2
    pure $ union x y
  Ou f1 f2 -> do
    x <- filter ((== Top) . snd) <$> solve f1
    y <- filter ((== Top) . snd) <$> solve f2
    pure $ intersect x y
  Implique f1 f2 -> do
    x <- solve f1
    y <- filter ((== Top) . snd) <$> solve f2
    pure $ intersect x y

a, b :: Proposition
a = Prop "a"
b = Prop "b"

example :: Formule
example =
  Et
    (P (Prop "a"))
    (P (Prop "b"))

-- >>> solve example
-- Just [(Prop "test",Top),(Prop "b",Top)]

main :: IO ()
main = putStrLn "Hello, Haskell!"
