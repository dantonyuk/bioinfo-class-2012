import Control.Monad
import Control.Monad.Random
import Data.Function
import Data.List
import Data.Maybe
import System.Random

chunks l = takeWhile ((l==) . length) . map (take l) . tails


gibbs n k t dnas = do
    let len = length (head dnas)
    is <- replicateM t (randomRIO (0, len-k))
    let ms = zipWith (\i -> take k . drop i) is dnas
        best = ms
        go 0 best _  = return best
        go n best ms = do
            j <- randomRIO (0, t-1)
            when (j>=t) $ error "WTF"
            let mss = minus j ms
                p = profile mss
                dna = dnas !! j
                dist = map (\kmer -> (kmer, pr p kmer)) $ chunks k dna
            msj <- fmap head . evalRandIO . sequence . repeat . fromList $ dist
            let newms = upd j msj ms
            go (n-1) (if score newms < score best then newms else best) newms
    go n ms ms

  where
    minus 0 (x:xs) = xs
    minus n (x:xs) = x : minus (n-1) xs

    upd 0 y (x:xs) = y:xs
    upd n y (x:xs) = x : upd (n-1) y xs
    upd n _ _ = error ("Wrong index: " ++ show n ++ " while t is " ++ show t)

    pr prof pat = product $ zipWith profProb pat prof

-- not a profile, it is a score table
profile = map (zip "ATGC" . normalize . eval) . transpose
  where
    eval ts = map (\l -> 1 + length (filter (l==) ts)) "ATGC"
    normalize ts = map (\t -> fromIntegral t / fromIntegral (sum ts)) ts


score xs = sum $ map hamming xs
  where
    consensus = map (fst . head . sorted) $ transpose xs

    hamming = length . filter not . zipWith (==) consensus

    sorted = sortBy cmpCount . map weight . group . sort

    weight xs = (head xs, length xs)

    cmpCount (_,x) (_,y) = y `compare` x


motifs k prof = map (mostProbable k prof)

mostProbable k prof dna = let (x:xs) = map (\kmer -> (kmer, prob kmer)) $ chunks k dna in fst $ findMax x xs
  where
    prob kmer = product $ zipWith profProb kmer prof
    findMax max [] = max
    findMax max (x:xs)
        | snd x > snd max = findMax x xs
        | otherwise       = findMax max xs

profProb n probs = fromJust $ lookup n probs -- laplace rule


main = do
    [k,t,n] <- fmap (map read . words) getLine
    dnas <- fmap lines getContents
    best <- gibbs n k t dnas
    print $ score best
    putStrLn . unlines $ best
