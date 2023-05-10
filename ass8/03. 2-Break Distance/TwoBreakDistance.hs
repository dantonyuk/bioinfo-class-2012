{-# LANGUAGE PatternGuards #-}

import Data.Foldable (find)
--import Data.List
import Data.List.Split
import Data.Maybe
import Data.Monoid
import qualified Data.Set as S

cyclic = sum . map length

canon (x,y) = (min x y, max x y)

toGraph = mconcat . map pairs
  where
    pairs xs@(x:xss) = S.fromList $ zipWith (curry canon) xs $ map negate (xss ++ [x])

blocks = go 0 Nothing
  where
    go n Nothing gr
        | S.null gr = n
        | otherwise = go n (Just v) nextGr 
      where
        Just ((v, _), nextGr) = S.minView gr
    go n (Just v) gr
        | Just vNext <- find (withVertex v) gr = go n (Just $ comp v vNext) (S.delete vNext gr)
        | otherwise                            = go (n + 1) Nothing gr
      where
        withVertex v (l,r) = v == l || v == r
        comp v (l,r)
            | v == l    = r
            | otherwise = l

readSeq :: String -> [[Int]]
readSeq = map (map read . splitOn " " . filter (`notElem` "()+")) . splitOn ")("

main = do
    seq1 <- fmap readSeq getLine
    seq2 <- fmap readSeq getLine
    let gr = toGraph $ seq1 ++ seq2
        c = cyclic seq1
        b = blocks gr
    print (c - b)

