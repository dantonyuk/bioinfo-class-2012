{-# LANGUAGE PatternGuards, ViewPatterns #-}

module Graph where

import Control.Applicative
import Data.List
import qualified Data.IntMap as M
import Data.Maybe
import Data.Monoid
import qualified Data.Set as S

type Vertex = Int

data Edge = Edge {
    from    :: Vertex,
    to      :: Vertex,
    weight  :: Int
} deriving Show

data DAG = DAG {
    nodes :: S.Set Vertex,
    edges :: [Edge]
} deriving Show

incoming :: DAG -> Vertex -> [Edge]
incoming (DAG ns es) vid = filter withTo es
  where
    withTo (to -> tov) = tov == vid

outcoming :: DAG -> Vertex -> [Edge]
outcoming (DAG ns es) vid = filter withFrom es
  where
    withFrom (from -> fromv) = fromv == vid

starting :: DAG -> [Vertex]
starting (DAG ns es) = S.toList $ S.filter (`notElem` tos) ns
  where
    tos = map to es

finishing :: DAG -> [Vertex]
finishing (DAG ns es) = S.toList $ S.filter (`notElem` froms) ns
  where
    froms = map from es

levels :: DAG -> [[Vertex]]
levels dag = ss : go ss (S.toList (nodes dag) \\ ss)
  where
    ss = starting dag
    go ns [] = []
    go ns rest = next : go next newRest
      where
        outs = S.fromList [ to e | n <- ns, e <- outcoming dag n ]
        checkParents = all (not . (`elem` rest) . from) . incoming dag
        cond n = S.member n outs && checkParents n
        (next, newRest) = partition cond rest
