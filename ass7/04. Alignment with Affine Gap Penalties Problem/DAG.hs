{-# LANGUAGE ViewPatterns, TupleSections #-}

module DAG where

import Data.Function (on)
import Data.List (maximumBy, init)
import Data.Maybe
import qualified Data.Map as M

data VertexType = Lower | Middle | Upper deriving (Eq, Ord, Show)

data Vertex = V {
    coords :: (Int,Int),
    vtype  :: VertexType
} deriving (Eq, Ord)

instance Show Vertex where
  show (V c Middle) = 'M' : show c
  show (V c Lower) = 'L' : show c
  show (V c Upper) = 'U' : show c

data Edge = Down { from :: Vertex }
          | Rite { from :: Vertex }
          | Diag { from :: Vertex }
          | Free { from :: Vertex, freeTo :: Vertex }
    deriving Show

data DAG = DAG {
    dagDim :: (Int, Int),
    edgeWeigth :: Edge -> Int
}

-- mkEdgeWeight 

source :: DAG -> Vertex
source dag = V (0,0) Middle

sink :: DAG -> Vertex
sink (DAG (n,m) _) = V (n,m) Middle

incoming :: DAG -> Vertex -> [Edge]
incoming dag@(DAG (n,m) _) v@(V (i,j) Lower)
    | i == 0    = []
    | i == 1    = [Free (V (i-1,j) Middle) v]
    | otherwise = [Free (V (i-1,j) Middle) v, Down (V (i-1, j) Lower)]
incoming dag@(DAG (n,m) _) v@(V (i,j) Upper)
    | j == 0    = []
    | j == 1    = [Free (V (i,j-1) Middle) v]
    | otherwise = [Free (V (i,j-1) Middle) v, Rite (V (i, j-1) Upper)]
incoming dag@(DAG (n,m) _) v@(V (0,0) Middle) = []
incoming dag@(DAG (n,m) _) v@(V (i,j) Middle)
    | i == 0    = [Free v{vtype=Upper} v]
    | j == 0    = [Free v{vtype=Lower} v]
    | otherwise = [Free v{vtype=Lower} v, Free v{vtype=Upper} v] ++ if i==0 || j==0 then [] else [Diag (V (i-1,j-1) Middle)]

levels :: DAG -> [[Vertex]]
levels dag@(DAG (n,m) _) = init $ go middles
  where
    middles = [[V (i, j) Middle | i <- [max 0 (s-m) .. min n s], let j = s-i] | s <- [0..n+m]]
    go xss = xss >>= \xs -> [xs, xs >>= outs]
    outs (V (i,j) Middle) = [V (i+1,j) Lower, V (i,j+1) Upper]

type PathCache = M.Map Vertex (Int, Edge)

evalDAG :: DAG -> PathCache
evalDAG dag@(DAG (n,m) weight) = go rest . M.fromList $ map (,(0,Diag (V (0,0) Middle))) ss
  where
    (ss:rest) = levels dag
    go :: [[Vertex]] -> PathCache -> PathCache
    go []          cache = cache
    go (next:rest) cache = go rest $ foldr (\n -> M.insert n (maxPath n)) cache next
      where
        get vertex = M.lookup vertex cache
        val edge@(from -> (get -> Just (len,_))) = (len + weight edge, edge)
        val edge = error $ "VAL: " ++ show edge
        valEdges vertex = map val (incoming dag vertex)
        maxPath vertex = maximumBy (compare `on` fst) ((-1000000000000000,undefined):(valEdges vertex))

back :: DAG -> PathCache -> [Edge]
back dag paths = reverse . go $ sink dag
  where
    go (V (0,0) Middle) = []
    go vertex = edge : go (from edge)
      where
        Just (_, edge) = M.lookup vertex paths
