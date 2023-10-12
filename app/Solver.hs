module Solver where

import qualified Data.Map.Strict as M
import qualified Data.Set        as S
import           Types

-- | Générer toutes les valuations possible (ensemble `Val`)
gen :: [Proposition] -> [Valuation]
gen ps =
  let l      = length ps
      perm 0 = []
      perm 1 = [[True], [False]]
      perm n = [p : t | p <- [True, False], t <- perm (n - 1)]
  in  map (M.fromList . zip ps) (perm l)

-- | Trouver toutes les propositions
findProp :: Formula -> [Proposition]
findProp =
  let go :: Formula -> S.Set Proposition
      go (P p)           = S.singleton p
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
  P p           -> vs M.! p
  Not f         -> not ( eval f vs)
  And f1 f2     -> eval f1 vs && eval f2 vs
  Or f1 f2      -> eval f1 vs || eval f2 vs
  Implies f1 f2 -> not (eval f1 vs) || eval f2 vs

-- | Trouver toutes les valuations qui satisfait une formule
solve :: Formula -> [Valuation]
solve f =
  let props = gen $ findProp f
      res   = map (eval f) props
      ts    = map fst . filter snd $ zip props res
  in  ts

showExamples :: [Formula] -> [IO ()]
showExamples fs = putStrLn . uncurry showFormule <$> zip [1 ..] fs
  where
    showFormule i f = unlines
      [ "exemple nº" ++ show i
      , "Formule : " ++  show f
      , "Contient les propositions: " ++ show (findProp f)
      , "Solutions: " ++ show (solve f)
      ]

-- Old examples
-- p = P $ Prop "p"
-- q = P $ Prop "q"
-- j = P $ Prop "j"
-- r = P $ Prop "r"
-- l = P $ Prop "l"
--
-- -- | exemple d’une liste de formule où chaque formule est connecté avec `Et`
-- s =
--   foldl1
--     And
--     [ Not j `Implies` r
--     , Not r `Implies` l
--     , j `Or` r `Or` l
--     , Not (j `And` r) `And` Not (j `And` l) `And` Not (r `And` l)
--     ]
--
-- main =
--   sequence $
--     showExamples
--       [ And Top p
--       , Or Top p
--       , Not p
--       , And p q
--       , And p p
--       , Or p q
--       , s
--       ]
