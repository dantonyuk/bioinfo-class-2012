{-# LANGUAGE ViewPatterns, TupleSections #-}

import DAG

import qualified Data.Map as M

main = do
    s1 <- getLine
    s2 <- getLine
    let n = length s1
        m = length s2
        weight (Diag (i,j)) = if s1!!i==s2!!j then 0 else -1
        weight _            = -1
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
