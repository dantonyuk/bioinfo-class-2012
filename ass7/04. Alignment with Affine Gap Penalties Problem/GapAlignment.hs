{-# LANGUAGE ViewPatterns, TupleSections #-}

import BLOSUM62
import DAG

import qualified Data.Map as M

main = do
    s1 <- getLine
    s2 <- getLine
    let n = length s1
        m = length s2
        weight (Diag (V (i,j) Middle)) = score (s1!!i) (s2!!j)
        weight (Free (V _ Middle) _) = -11
        weight Free{} = 0
        weight Down{} = -1
        weight Rite{} = -1
        dag = DAG (n,m) weight
        paths = evalDAG dag
        lpath = back dag paths
        Just (len,_) = M.lookup (V (n,m) Middle) paths
    print len
    putStrLn (colAlignment s1 lpath)
    putStrLn (rowAlignment s2 lpath)
  where
    colAlignment [] [] = []
    colAlignment (c:cs) ((Free (V _ Middle) (V _ Lower)):path) = c : colAlignment cs path
    colAlignment cs ((Free (V _ Middle) (V _ Upper)):path) = '-' : colAlignment cs path
    colAlignment cs (Free{}:path) = colAlignment cs path
    colAlignment cs (Rite{}:path) = '-' : colAlignment cs path
    colAlignment (c:cs) (_:path) = c : colAlignment cs path
    colAlignment cs path = error $ cs ++ ": " ++ show path

    rowAlignment [] [] = []
    rowAlignment (c:cs) ((Free (V _ Middle) (V _ Upper)):path) = c : rowAlignment cs path
    rowAlignment cs ((Free (V _ Middle) (V _ Lower)):path) = '-' : rowAlignment cs path
    rowAlignment cs (Free{}:path) = rowAlignment cs path
    rowAlignment cs (Down{}:path) = '-' : rowAlignment cs path
    rowAlignment (c:cs) (_:path) = c : rowAlignment cs path
