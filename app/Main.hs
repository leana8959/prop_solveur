module Main where

import           Control.Arrow   (Arrow (second))
import           Data.List
import qualified Data.Map.Strict as M
import qualified Data.Set        as S

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

type Valuation = M.Map Proposition Bool

-- | Générer toutes les valuations possible (ensemble `Val`)
gen :: [Proposition] -> [Valuation]
gen ps =
  let l      = length ps
      perm 0 = []
      perm 1 = [[True], [False]]
      perm n = [p : t | p <- [True, False], t <- perm (n - 1)]
  in  map (M.fromList . zip ps) (perm l)

-- | Trouver toutes les propositions
findProp :: Formule -> [Proposition]
findProp =
  let go :: Formule -> S.Set Proposition
      go (P p)            = S.singleton p
      go (Non f)          = go f
      go (Et f1 f2)       = S.union (go f1) (go f2)
      go (Ou f1 f2)       = S.union (go f1) (go f2)
      go (Implique f1 f2) = S.union (go f1) (go f2)
      go _                = S.empty
  in S.toList . go

-- | Evaluer une formule étant donné une valuation
eval :: Formule -> Valuation -> Bool
eval f vs = case f of
  Top            -> True
  Bottom         -> False
  P p            -> vs M.! p
  Non f          -> not ( eval f vs)
  Et f1 f2       -> eval f1 vs && eval f2 vs
  Ou f1 f2       -> eval f1 vs || eval f2 vs
  Implique f1 f2 -> not (eval f1 vs) || eval f2 vs

-- | Trouver toutes les valuations qui satisfait une formule
solve :: Formule -> [Valuation]
solve f =
  let props = gen $ findProp f
      res = map (eval f) props
      ts = map fst . filter snd $ zip props res
  in  ts

showExamples :: [Formule] -> [IO ()]
showExamples fs = putStrLn . uncurry showFormule <$> zip [1 ..] fs
  where
    showFormule i f = unlines
      [ "exemple nº" ++ show i
      , "Formule : " ++  show f
      , "Contient les propositions: " ++ show (findProp f)
      , "Solutions: " ++ show (solve f)
      ]

p = P $ Prop "p"
q = P $ Prop "q"
j = P $ Prop "j"
r = P $ Prop "r"
l = P $ Prop "l"

-- | exemple d’une liste de formule où chaque formule est connecté avec `Et`
s =
  foldl1
    Et
    [ Non j `Implique` r
    , Non r `Implique` l
    , j `Ou` r `Ou` l
    , Non (j `Et` r) `Et` Non (j `Et` l) `Et` Non (r `Et` l)
    ]

main =
  sequence $
    showExamples
      [ Et Top p
      , Ou Top p
      , Non p
      , Et p q
      , Ou p q
      , Et p p
      , s
      ]
