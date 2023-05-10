{-# LANGUAGE ViewPatterns, PatternGuards, TupleSections #-}

import Data.List
import Data.List.Split
import qualified Data.Map as M
import Debug.Trace

--eulerianCycle :: [(Int, Int)] -> [(Int, Int)]
eulerianCycle edges = reverse $ go (deleteEdge unEdge edges) [unEdge]
  where
    (unbalanced:_) = M.keys $ M.filter (>0) both
      where
        lefts  = foldr (go 1 fst) M.empty edges
        rights = foldr (go (-1) snd) M.empty edges
        both   = M.unionWith (+) lefts rights

        go shift f = M.alter (Just . maybe shift (+shift)) . f

    Just unEdge = findEdge unbalanced edges

    deleteEdge e (e':es)
        | e == e'   = es
        | otherwise = e' : deleteEdge e es

    findEdge l = find ((l==) . fst)

    go [] cs = cs
    go es cs@((_,r):_)
        | Just e <- findEdge r es = go (deleteEdge e es) (e:cs)
        | otherwise               = loop es cs
--        | otherwise               = trace ("CS: " ++ showCs cs ++ "\nES: " ++ showEs es ++ "\n-------------------------------------------") $  loop es cs

    loop es [(l,r)]
        | Just e <- findEdge r es = go (deleteEdge e es) (e:[(l,r)])
        | Just e <- findEdge l es = go (deleteEdge e es) [(l,r),e]
    loop es cs@(c@(_,r):cs')
        | Just e <- findEdge r es = go (deleteEdge e es) (e:cs)
        | otherwise               = c : loop es cs'

--    showCs = showEs . reverse
--    showEs [] = ""
--    showEs es@((l,_):_) = show l ++ concatMap ((" -> " ++) . show . snd) es

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

