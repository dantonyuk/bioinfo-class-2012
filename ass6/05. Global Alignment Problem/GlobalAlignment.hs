{-# LANGUAGE ViewPatterns, TupleSections #-}

import DAG
import BLOSUM62

import Data.Maybe (fromJust)
import qualified Data.Map as M

main = do
    s1 <- getLine
    s2 <- getLine
    let n = length s1
        ls1 = M.fromList $ zip [0..] s1
        ls2 = M.fromList $ zip [0..] s2
        m = length s2
        weight (Diag (i,j)) = score (fromJust $ M.lookup i ls1) (fromJust $ M.lookup j ls2) -- score (s1!!i) (s2!!j)
        weight _            = -5
        dag = DAG (n,m) weight
        paths = evalDAG dag
        lpath = back dag paths
        Just (len,_) = M.lookup (n,m) paths
    print len
    putStrLn (colAlignment s1 lpath)
    putStrLn (rowAlignment s2 lpath)
  where
    colAlignment [] [] = []
    colAlignment cs (Rite{}:path) = '-' : colAlignment cs path
    colAlignment (c:cs) (_:path) = c : colAlignment cs path

    rowAlignment [] [] = []
    rowAlignment cs (Down{}:path) = '-' : rowAlignment cs path
    rowAlignment (c:cs) (_:path) = c : rowAlignment cs path
