import Data.List
import Data.Maybe

chunks l = takeWhile ((l==) . length) . map (take l) . tails

mostProbable k prof dna = fst $ maximumBy (\x y -> snd x `compare` snd y) $ map (\kmer -> (kmer, prob kmer)) $ chunks k dna
  where
    prob kmer = product $ zipWith profProb kmer prof
      where
        profProb n probs = fromJust $ lookup n probs

profile columns matrix = map (zip columns) matrix

main = do
    dna <- getLine
    k <- fmap read getLine
    columns <- fmap (map head . words) getLine
    matrix <- fmap (map (map read . words) . lines) getContents
    let prof :: [[(Char, Double)]]
        prof = profile columns matrix
    putStrLn $ mostProbable k prof dna
