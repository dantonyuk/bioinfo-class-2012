import BLOSUM62

import Control.Monad
import Data.Function
import Data.List
import Data.Maybe (fromJust)
import qualified Data.Map as M

penalty = 5

main = do
    s1 <- getLine
    s2 <- getLine
    let (fi,fj) = getMiddleNode getScore s1 s2
        (ti,tj) = getMiddleEdge getScore s1 s2 (fi,fj)
        n = length s1
        m = length s2
        ls1 = M.fromList $ zip [0..] s1
        ls2 = M.fromList $ zip [0..] s2
        getScore i j = score (fromJust $ M.lookup i ls1) (fromJust $ M.lookup j ls2)
        startCol = take (length s1 + 1) $ iterate (\x -> x - penalty) 0
        mid = m `div` 2
        fromSource = foldl' (getNextCol s1) startCol (take mid s2)
    putStrLn $ "(" ++ show fi ++ ", " ++ show fj ++ ") (" ++ show ti ++ ", " ++ show tj ++ ")"
  where
    getMiddleNode getScore s1 s2 = (\(_,i) -> (i,mid)) $ maximumBy compareNodes $
        zipWith3 (\l r i -> (l + r, i)) fromSource toSink [0..]
      where
        n = length s1
        m = length s2
        startCol = take (n + 1) $ iterate (\x -> x - penalty) 0
        mid = m `div` 2
        dif = m - mid
        diag i = getScore i mid
        fromSource = foldl' (getNextCol s1) startCol (take mid s2)
        toSink = back $ foldl' (getNextCol $ reverse s1) startCol (take dif $ reverse s2)
          where
            back [] = []
            back (x:xs) = reverse xs ++ [x]
        compareNodes (v1,i1) (v2,i2) = compare (v1,-i1) (v2,-i2)

    getMiddleEdge getScore s1 s2 (mi,mj) =
        snd $ maximumBy (compare `on` fst) [
            (right - penalty, (mi, mj+1)),
--            (down - penalty, (mi+1,mj)),
            (diag + diagScore, (mi+1,mj+1))]
      where
        n = length s1
        diagScore = getScore mi mj
        right = toSink mi (mj+1)
        down  = toSink (mi+1) mj
        diag  = toSink (mi+1) (mj+1)
        toSink i j = last $ foldl' (getNextCol . reverse $ drop i s1) startCol (reverse $ drop j s2)
          where
            startCol = take (n + 1 - i) $ iterate (\x -> x - penalty) 0
        compareEdges (v1,((f1,_),_)) (v2,((f2,_),_)) = compare (v1,-f1) (v2,-f2)

    getNextCol str prevCol@(val:_) char = result
      where
        result = (val - penalty) : go str prevCol result
        go (char2:str) (c1:ps@(c2:_)) (c3:rest) =
            maximum [
                c1 + score char char2,
                c2 - penalty,
                c3 - penalty] : go str ps rest
        go [] [_] _ = []
