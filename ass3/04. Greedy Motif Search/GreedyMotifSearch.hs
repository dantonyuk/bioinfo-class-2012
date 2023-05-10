import Data.Function
import Data.List
import Data.Maybe

chunks l = takeWhile ((l==) . length) . map (take l) . tails

greedy k t dnas = findMin firstMs . map prepareMotifs . chunks k $ head dnas
  where
    firstMs = map (head . chunks k) dnas
    prepareMotifs pat = foldl go [pat] . take (t-1) $ tail dnas
      where
        go ms dna = ms ++ [mostProbable k (profile ms) dna]
    findMin min [] = min
    findMin min (x:xs)
        | score x < score min = findMin x xs
        | otherwise           = findMin min xs


-- not a profile, it is a score table
profile = map go . transpose
  where
    go = map (\xs -> (head xs, length xs + 1)) . group . sort


score = sum . map go . transpose
  where
    go = sum . map snd . tail . sortBy cmpCount . map weight . group . sort
    weight xs = (head xs, length xs)
    cmpCount (_,x) (_,y) = y `compare` x


mostProbable k prof dna = let (x:xs) = map (\kmer -> (kmer, prob kmer)) $ chunks k dna in fst $ findMax x xs
  where
    prob kmer = product $ zipWith profProb kmer prof
    findMax max [] = max
    findMax max (x:xs)
        | snd x > snd max = findMax x xs
        | otherwise       = findMax max xs

probs k prof dna = map (\kmer -> (kmer, prob kmer)) $ chunks k dna
  where
    prob kmer = product $ zipWith profProb kmer prof

--profProb n probs = fromMaybe 0 $ lookup n probs
profProb n probs = maybe 1 (1+) $ lookup n probs -- laplace rule


test = greedy 3 5 ["GGCGTTCAGGCA", "AAGAATCAGTCA", "CAAGGAGTTCGC", "CACGTCAATCAC", "CAATAATATTCG"]


main = do
    [k,t] <- fmap (map read . words) getLine
    dnas <- fmap lines getContents
    putStrLn . unlines $ greedy k t dnas
