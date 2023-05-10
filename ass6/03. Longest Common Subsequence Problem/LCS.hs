{-# LANGUAGE ViewPatterns, TupleSections #-}

import DAG

main = do
    s1 <- getLine
    s2 <- getLine
    let n = length s1
        m = length s2
        weight (Diag (i,j))
            | s1!!i == s2!!j = 1
        weight _             = 0
        dag = DAG (n,m) weight
        paths = evalDAG dag
        lpath = [ s1!!i | Diag (i,j) <- back dag paths, s1!!i == s2!!j]
    putStrLn lpath
