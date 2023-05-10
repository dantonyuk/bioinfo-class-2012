import Data.List

{-
    GREEDYSORTING(P)
        approxReversalDistance ← 0
        for k = 1 to |P|
            if element k is not sorted
                apply the k-sorting reversal to P
                approxReversalDistance ← approxReversalDistance + 1
            if k-th element of P is −k
                apply the reversal flipping the k-th element of P
                approxReversalDistance ← approxReversalDistance + 1
        return approxReversalDistance
-}


greedySorting p = go 1 p
  where
    len = length p
    go i p | i == len + 1 = []
    go i p
        | r == i = go (i+1) p
        | otherwise = res : go i res
      where
        res | r == -i   = ls ++ (-r:rs)
            | otherwise = ls ++ ((-v) : rev (r:fs)) ++ rest
        (ls,(r:rs)) = splitAt (i-1) p
        (fs,(v:rest)) = break (\x -> abs x == i) rs
        rev = reverse . map negate

main = do
    ps <- fmap (map read . words . filter (`notElem` "+()")) getLine
    mapM_ (putStrLn . (\s -> "(" ++ s ++ ")") . intercalate " " . map showN) $ greedySorting ps
  where
    showN n | n > 0 = '+' : show n
            | True  = show n
