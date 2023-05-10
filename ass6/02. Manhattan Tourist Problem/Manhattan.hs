{-# LANGUAGE ViewPatterns, TupleSections #-}

import Graph

import Control.Monad
import Data.Function
import qualified Data.IntMap as M
import Data.List
import Data.Maybe
import qualified Data.Set as S

type PathCache = M.IntMap (Int, Vertex)

evalDAG :: DAG -> PathCache
evalDAG dag = go ss rest . M.fromList $ map (,(0,0)) ss
  where
    (ss:rest) = levels dag
    go :: [Vertex] -> [[Vertex]] -> PathCache -> PathCache
    go _    []          cache = cache
    go curr (next:rest) cache = go next rest $ foldr (\n -> M.insert n (maxPath n)) cache next
      where
        get id = M.lookup id cache
        val (Edge fv@(get -> Just (prev, _)) _ w) = (prev + w, fv)
        maxPath node = maximumBy (compare `on` fst) ((0,0) : map val (incoming dag node))


back :: DAG -> M.IntMap (Int, Vertex) -> [Vertex]
back dag paths = reverse $ go last
  where
    (last:_) = finishing dag
    go 0 = [0]
    go node = node : go prev
      where
        Just (_, prev) = M.lookup node paths


main = do
    n <- fmap read getLine
    m <- fmap read getLine
    let nodes = S.fromList [v i j | i <- [0..n], j <- [0..m]]
        v i j = i*(m+1) + j
    downs <- fmap concat $ forM [0..n-1] $ \i -> do
        ws <- fmap (map read . words) getLine
        let edges = zipWith mkEdge [0..] ws
            mkEdge j w = Edge (v i j) (v (i+1) j) w
        return edges
    "-" <- getLine
    rights <- fmap concat $ forM [0..n] $ \i -> do
        ws <- fmap (map read . words) getLine
        let edges = zipWith mkEdge [0..] ws
            mkEdge j w = Edge (v i j) (v i (j+1)) w
        return edges
    let dag = DAG nodes (downs ++ rights)
--    print dag
        paths = evalDAG dag
        Just (lp,_) = M.lookup (v n m) paths
    print lp
