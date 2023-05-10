{-# LANGUAGE ViewPatterns, PatternGuards, TupleSections #-}

import Graph

import Control.Monad
import Data.Function
import qualified Data.IntMap as M
import Data.List
import Data.List.Split
import Data.Maybe
import qualified Data.Set as S

type PathCache = M.IntMap (Int, Vertex)

evalDAG :: DAG -> Vertex -> PathCache
evalDAG dag source = go [source] rest $ M.fromList [(source,(0,source))]
  where
    (_:rest) = dropWhile (source `notElem`) $ levels dag
    go :: [Vertex] -> [[Vertex]] -> PathCache -> PathCache
    go _    []          cache = cache
    go curr (next:rest) cache = go next rest $ foldr add cache next
      where
        get id = M.lookup id cache
        val (Edge fv _ w)
            | Just (prev, _) <- get fv = (prev + w, fv)
            | otherwise                = (-1, fv)
        ins node = map val . filter ((`M.member` cache) . from) $ incoming dag node
        maxPath node = maximumBy (compare `on` fst) ((0,0) : map val (incoming dag node))
        add n cs
            | n == source = M.insert n mp cs
            | mp == (0,0) = cs
            | otherwise   = M.insert n mp cs
          where
            mp = maxPath n


back :: DAG -> Vertex -> Vertex -> M.IntMap (Int, Vertex) -> [Vertex]
back dag source sink paths = reverse $ go sink
  where
    go vertex
        | vertex == source                          = [source]
        | Just (_, prev) <- M.lookup vertex paths   = vertex : go prev
        | otherwise                                 = [vertex]


main = do
    source <- fmap read getLine
    sink <- fmap read getLine
    edges <- fmap (map readEdge . lines) getContents
    let vertices = S.union (S.fromList $ map from edges) (S.fromList $ map to edges)
        dag = DAG vertices edges
        paths = evalDAG dag source
        Just (len,_) = M.lookup sink paths
        lpath = back dag source sink paths
    --print dag
    --print paths
    print len
    putStrLn $ intercalate "->" $ map show $ take 50 lpath
  where
    readEdge s = Edge (read f) (read t) (read w)
      where
        [f,e] = splitOn "->" s
        [t,w] = splitOn ":" e
