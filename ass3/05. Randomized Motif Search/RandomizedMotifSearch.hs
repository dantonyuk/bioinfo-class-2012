import Control.Monad
import Data.Function
import Data.List
import Data.Maybe
import System.Random

chunks l = takeWhile ((l==) . length) . map (take l) . tails


randomized k t dnas best start = run best $ iterate go start
  where
    go ms = motifs k (profile ms) dnas

    run Nothing (ms:mss) = run (Just ms) mss
    run (Just best) (ms:mss)
        | score ms < score best = run (Just ms) mss
        | otherwise             = best

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
    [k,t] <- fmap (map read . words) getLine
    dnas <- fmap lines getContents
    let len = length (head dnas)
        go 0 (Just best) = return best
        go n best = do
            is <- replicateM t (randomRIO (0, len-k))
            let start = zipWith (\i -> take k . drop i) is dnas
                motifs = randomized k t dnas best start
            go (n-1) (Just motifs)
    motifs <- go 5000 Nothing
    print $ score motifs
    putStrLn . unlines $ motifs
