import BLOSUM62

import Data.Function
import Data.List
import qualified Data.Map as M
import Data.Maybe

import Debug.Trace

penalty = 5

main = do
    s1 <- getLine
    s2 <- getLine
    let n = length s1
        m = length s2
        ls1 = M.fromList $ zip [0..] s1
        ls2 = M.fromList $ zip [0..] s2

        getScore io jo i j = score (fromJust $ M.lookup (i + io) ls1) (fromJust $ M.lookup (j + jo) ls2)

        lsa top bottom left right
            | left == right = map (\i -> ((i,left), (i+1,left))) [top .. bottom-1]
--            | top == bottom = map (\i -> ((top,i), (top,i+1))) [left .. right-1]
            | midNode < top || midNode > bottom = error $ show (top,bottom,midNode)
            | otherwise     =
                lsa top midNode left middle ++
                [(node,edge)] ++
                lsa newMidNode bottom newMiddle right
          where
            middle = (left + right) `div` 2
            getS l r = take (r-l) . drop l
            ss1 = getS top bottom s1
            ss2 = getS left right s2
            correct (i, j) = (top + i, left + j)
            offMidNode = getMiddleNode (getScore top left) ss1 ss2
            node@(midNode,_) = correct offMidNode
            edge = correct $ getMiddleEdge (getScore top left) ss1 ss2 offMidNode
            hasDown  = fst edge /= fst node
            hasRight = snd edge /= snd node
            newMiddle
                | hasRight   = middle + 1
                | otherwise = middle
            newMidNode
                | hasDown  = midNode + 1
                | otherwise = midNode
    let lpath = lsa 0 n 0 m
--    print $ verify lpath
--    print $ verify2 lpath
    print $ value (getScore 0 0) s1 s2 lpath
--    putStrLn $ showPath lpath
    putStrLn $ colAlignment s1 lpath
    putStrLn $ rowAlignment s2 lpath
  where
    verify2 lpath = and $ zipWith go lpath $ tail lpath
      where
        go (_,x) (y,_) = x == y

    verify = all checked
      where
        checked ((fi,fj),(ti,tj)) = (ti-fi) `elem` [0,1] && (tj-fj) `elem` [0,1] && not (ti==fi && tj==fj)

    showPath path = map (uncurry f) path
      where
        f (fi,fj) (ti,tj)
            | ti == fi + 1 && tj == fj + 1 = 'G'
            | ti == fi + 1                 = 'D'
            | otherwise                    = 'R'

    colAlignment [] [] = []
    colAlignment cs@(~(c:css)) (((fi,fj),(ti,tj)):path)
        | fi == ti  = '-' : colAlignment cs path
        | otherwise = c : colAlignment css path

    rowAlignment [] [] = []
    rowAlignment cs@(~(c:css)) (((fi,fj),(ti,tj)):path)
        | fj == tj  = '-' : rowAlignment cs path
        | otherwise = c : rowAlignment css path

    value getScore s1 s2 = sum . map go
      where
        go ((fi,fj),(ti,tj))
            | fi==ti || fj==tj = -penalty
            | otherwise        = getScore fi fj

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
        compareNodes (v1,i1) (v2,i2) = compare (v1,i1) (v2,i2)

    getMiddleEdge getScore s1 s2 (mi,mj)
        | n <= mi = (mi, mj+1)
        | otherwise =
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
