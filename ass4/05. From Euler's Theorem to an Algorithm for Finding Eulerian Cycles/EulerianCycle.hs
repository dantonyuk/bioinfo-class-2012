{-# LANGUAGE ViewPatterns, PatternGuards, TupleSections #-}

import Data.List
import Data.List.Split

eulerianCycle :: [(Int, Int)] -> [(Int, Int)]
eulerianCycle (edge:edges) = reverse $ go edges [edge]
  where
    deleteEdge e (e':es)
        | e == e'   = es
        | otherwise = e' : deleteEdge e es

    findEdge l = find ((l==) . fst)

    go [] cs = cs
    go es cs@((_,r):_)
        | Just e <- findEdge r es = go (deleteEdge e es) (e:cs)
        | otherwise               = loop es cs

    loop es cs@(c@(_,r):cs')
        | Just e <- findEdge r es = go (deleteEdge e es) (e:cs)
        | otherwise               = c : loop es cs'

main = do
    edges <- fmap (concatMap toAdjacent . lines) getContents
    putStrLn . intercalate "->" . map show . toCycle $ eulerianCycle edges
  where
    toAdjacent :: String -> [(Int, Int)]
    toAdjacent s = map ((read l,) . read) rs
      where
        [l,r] = splitOn " -> " s
        rs = splitOn "," r
    toCycle cs@((l,_):_) = l : map snd cs

