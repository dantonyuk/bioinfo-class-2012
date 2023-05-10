import Data.List
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S

unq = S.toList . S.fromList

levels (k,l,m) = go start
  where
    start = [(0,0,0)]
    go xs
        | null splice = []
        | otherwise   = splice : go splice
      where
        splice = unq $ xs >>= inc
    inc (x,y,z) = filter inCube [(x+1,y,z), (x,y+1,z), (x,y,z+1)]
    inCube (x,y,z) = x<=k && y <=l && z<=m

eval s1 s2 s3 = (finalScore, reverse . ((l1,l2,l3):) $ backPath (l1,l2,l3))
  where
    l1 = length s1
    l2 = length s2
    l3 = length s3

    scores :: M.Map (Int,Int,Int) Int
    scores = foldl' f (M.singleton (0,0,0) 0) $ levels (l1,l2,l3)
      where
        f m xs = foldl' (flip g) m xs
        g v@(i,j,k) = M.insert v . maximum $ catMaybes [
            withPrev (i-1,j,k) False, withPrev (i,j-1,k) False, withPrev (i,j,k-1) False,
            withPrev (i-1,j-1,k) False, withPrev (i,j-1,k-1) False, withPrev (i-1,j,k-1) False,
            withPrev (i-1,j-1,k-1) True]

    withPrev v@(i,j,k) check
        | any (<0) [i,j,k] = Nothing
        | check            = Just (s + same)
        | otherwise        = Just s
      where
        Just s = M.lookup v scores
        same
            | s1 !! i == s2 !! j && s2 !! j == s3 !! k = 1
            | otherwise                                = 0

    Just finalScore = M.lookup (l1,l2,l3) scores

    backPath (0,0,0) = []
    backPath vertex = let prev_vertex = prev vertex in prev_vertex : backPath prev_vertex
      where
        prevWithScore v check = (v, withPrev v check)
        prev v@(i,j,k) = head [ pv | (pv, Just psc) <- ps, psc == sc ]
          where
            ps = [
                prevWithScore (i-1,j-1,k-1) True,
                prevWithScore (i-1,j-1,k) False, prevWithScore (i-1,j,k-1) False, prevWithScore (i,j-1,k-1) False,
                prevWithScore (i-1,j,k) False, prevWithScore (i,j-1,k) False, prevWithScore (i,j,k-1) False]
            Just sc = M.lookup v scores


main = do
    s1 <- getLine
    s2 <- getLine
    s3 <- getLine
    let (final, path) = eval s1 s2 s3
        align [] [] = []
        align s@(~(c:cs)) ((f,t):path)
            | f == t    = '-' : align s path
            | otherwise = c : align cs path
        showPath f s = let ps = map f path in align s $ zip ps (tail ps)
    print final
--    print path
    putStrLn $ showPath (\(x,_,_) -> x) s1
    putStrLn $ showPath (\(_,x,_) -> x) s2
    putStrLn $ showPath (\(_,_,x) -> x) s3

