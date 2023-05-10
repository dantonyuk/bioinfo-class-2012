{-# LANGUAGE ViewPatterns, PatternGuards, TupleSections, ScopedTypeVariables #-}

import Data.List
import Data.List.Split
import qualified Data.Map as M
import Debug.Trace

eulerianCycle edges = reverse $ go (deleteEdge unEdge edges) [unEdge]
  where
    unbalanced = M.keys $ M.filter (>0) both
      where
        lefts  = fmap length edges
        rights = foldr go M.empty $ concat $ M.elems edges
        both   = M.unionWith (+) lefts rights

        go = M.alter (Just . maybe (-1) pred)

    Just unEdge | null unbalanced = Just (l, r)
                | otherwise       = findEdge (head unbalanced) edges
      where
        ((l,(r:_)):_) = M.toList edges

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
    d :: Int <- fmap read getLine
    edges <- fmap (map toEdge . lines) getContents
    putStrLn . toCycle d . eulerianCycle $ foldr mkEdge M.empty edges
  where
    mkEdge (l,r) = M.alter (Just . maybe [r] (r:)) l

    toEdge s = ((init l, init r), (tail l, tail r))
      where
        [l,r] = splitOn "|" s

    toCycle d cs@(((l1,l2),_):_) = take (length l1 + d + 1) seq1 ++ seq2
      where
        seq1 = l1 ++ map (last . fst . snd) cs
        seq2 = l2 ++ map (last . snd . snd) cs
