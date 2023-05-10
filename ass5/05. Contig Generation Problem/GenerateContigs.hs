{-# LANGUAGE TupleSections, PatternGuards #-}

import Data.List
import Data.Map (Map)
import qualified Data.Map as M

-- Pattern
type P = String

contigs :: Map P [P] -> [[P]]
contigs edges = concatMap path . filter (not . unbranching) $ M.keys edges
  where
    reversed = foldr mkEdge M.empty . concatMap fromEdge $ M.toList edges
      where
        fromEdge (x,xs) = map (,x) xs
        mkEdge (l,r) = M.alter (Just . maybe [r] (r:)) l

    unbranching node = checkAdj edges && checkAdj reversed
      where
        checkAdj g = not (null follows) && null (tail follows) -- length follows == 1
          where
            follows = M.findWithDefault [] node g

    path from
        | Just [r] <- next = map (from:) (continue r)
        | Just rs  <- next = map (from:) (concatMap continue rs)
        | otherwise        = [[from]]
      where
        next = M.lookup from edges
        continue node
            | unbranching node = path node
            | otherwise        = [[node]]

kmersToEdges :: [P] -> Map P [P]
kmersToEdges = foldr go M.empty
  where
    go kmer = M.alter (Just . maybe [r] (r:)) l
      where
        l = init kmer
        r = tail kmer

pathToSeq :: [P] -> P
pathToSeq xs@(x:_) = init x ++ map last xs

main = do
    kmers <- fmap lines getContents
    putStrLn . unlines . sort . map pathToSeq . contigs $ kmersToEdges kmers
    --putStrLn . intercalate " " . sort . map pathToSeq . contigs $ kmersToEdges kmers
    --print . contigs $ kmersToEdges kmers
