{-# LANGUAGE ViewPatterns, TupleSections #-}

import DAG
import PAM250

import qualified Data.Map as M

main = do
    s1 <- getLine
    s2 <- getLine
    let n = length s1
        m = length s2
        weight (Diag (i,j)) = score (s1!!i) (s2!!j)
        weight Free{}       = 0
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
    colAlignment cs ((Free (0,0) (i,_)):path) = colAlignment (drop i cs) path
    colAlignment cs (Free{}:path) = []
    colAlignment cs (Rite{}:path) = '-' : colAlignment cs path
    colAlignment (c:cs) (_:path) = c : colAlignment cs path

    rowAlignment [] [] = []
    rowAlignment cs ((Free (0,0) (_,j)):path) = rowAlignment (drop j cs) path
    rowAlignment cs (Free{}:path) = []
    rowAlignment cs (Down{}:path) = '-' : rowAlignment cs path
    rowAlignment (c:cs) (_:path) = c : rowAlignment cs path
