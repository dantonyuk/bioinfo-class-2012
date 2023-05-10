{-# LANGUAGE ViewPatterns, PatternGuards, TupleSections #-}

import Data.List
import Data.List.Split
import qualified Data.IntMap as M
import Debug.Trace

eulerianCycle :: M.IntMap [Int] -> [(Int, Int)]
eulerianCycle edges = reverse $ go (deleteEdge (0,0) edges) [(0,0)]
  where
{-
    unbalanced = M.keys $ M.filter (>0) both
      where
        lefts  = fmap length edges
        rights = foldr go M.empty $ concat $ M.elems edges
        both   = M.unionWith (+) lefts rights

        go = M.alter (Just . maybe (-1) pred)

    Just unEdge | null unbalanced = Just 0
                | otherwise       = findEdge (head unbalanced) edges
-}
    deleteEdge (l,r) = M.alter go l
      where
        go Nothing      = error "No edge found"
        go (Just [])    = error "No edge found"
        go (Just [x])
            | x == r    = Nothing
            | True      = error "No edge found"
        go (Just xs)
            | r `elem` xs  = Just (r `delete` xs)
            | otherwise     = error $ ("No edge found " ++ show (l,r))

    findEdge l es
        | Just xs <- M.lookup l es = Just (l, head xs)
        | otherwise                = Nothing

    go es cs@((_,r):_)
        | M.null es               = cs
        | Just e <- findEdge r es = go (deleteEdge e es) (e:cs)
        | otherwise               = loop es cs

    loop es [(l,r)]
        | Just e <- findEdge r es = go (deleteEdge e es) (e:[(l,r)])
        | Just e <- findEdge l es = go (deleteEdge e es) [(l,r),e]
    loop es cs@(c@(_,r):cs')
        | Just e <- findEdge r es = go (deleteEdge e es) (e:cs)
        | otherwise               = c : loop es cs'

main = do
    count <- fmap read getLine
    let nmax = 2^(count-1)
        edges = foldr mkEdges M.empty [0..nmax-1]
        mkEdges n = M.insert n [(2*n) `mod` nmax, (2*n + 1) `mod` nmax]
    putStrLn . toCycle count $ eulerianCycle edges
  where
    toCycle count cycle = (replicate (count-1) '0') ++ map (bin . snd) (take (length cycle - count + 1) cycle)
    bin n | even n    = '0'
          | otherwise = '1'
