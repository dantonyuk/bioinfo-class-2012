{-# LANGUAGE ViewPatterns, TupleSections #-}

module DAG where

import Data.Function (on)
import Data.List (maximumBy)
import Data.Maybe
import qualified Data.Map as M

type Vertex = (Int,Int)

data Edge = Down { from :: Vertex }
          | Rite { from :: Vertex }
          | Diag { from :: Vertex }
    deriving Show

to :: Edge -> Vertex
to (Down (i,j)) = (i+1, j)
to (Rite (i,j)) = (i, j+1)
to (Diag (i,j)) = (i+1, j+1)

data DAG = DAG {
    dagDim :: (Int, Int),
    edgeWeigth :: Edge -> Int
}

source :: DAG -> Vertex
source dag = (0,0)

sink :: DAG -> Vertex
sink (DAG (n,m) _) = (n,m)

incoming :: DAG -> Vertex -> [Edge]
incoming dag (i,j) = catMaybes [
    if i==0 then Nothing else Just $ Down (i-1, j),
    if j==0 then Nothing else Just $ Rite (i, j-1),
    if i==0 || j==0 then Nothing else Just $ Diag (i-1, j-1)]

outcoming :: DAG -> Vertex -> [Edge]
outcoming dag@(DAG (n,m) _) (i,j) = catMaybes [
    if i==n then Nothing else Just $ Down (i, j),
    if j==m then Nothing else Just $ Rite (i, j),
    if i==n || j==m then Nothing else Just $ Diag (i, j)]

levels :: DAG -> [[Vertex]]
levels dag@(DAG (n,m) _) =
    [[(i, j) | i <- [max 0 (s-m) .. min n s], let j = s-i] | s <- [0..n+m]]

type PathCache = M.Map Vertex (Int, Edge)

evalDAG :: DAG -> PathCache
evalDAG dag@(DAG (n,m) weight) = go ss rest . M.fromList $ map (,(0,Diag (0,0))) ss
  where
    (ss:rest) = levels dag
    go :: [Vertex] -> [[Vertex]] -> PathCache -> PathCache
    go _    []          cache = cache
    go curr (next:rest) cache = go next rest $ foldr (\n -> M.insert n (maxPath n)) cache next
      where
        get vertex = M.lookup vertex cache
        val edge@(from -> (get -> Just (len,_))) = (len + weight edge, edge)
        valEdges vertex = map val (incoming dag vertex)
        maxPath vertex = maximumBy (compare `on` fst) ((0,Diag (0,0)) : valEdges vertex)

back :: DAG -> PathCache -> [Edge]
back dag paths = reverse . go $ sink dag
  where
    go (0,0) = []
    go vertex = edge : go (from edge)
      where
        Just (_, edge) = M.lookup vertex paths
