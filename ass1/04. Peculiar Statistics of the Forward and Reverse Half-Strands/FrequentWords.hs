{-# LANGUAGE ViewPatterns #-}
import Data.List
import qualified Data.Map as M

chunks l = takeWhile ((l==) . length) . map (take l) . tails

{-
mismatches t1 t2 = length . filter id $ zipWith (/=) t1 t2

patternIndices d sub str = [ i | (i,s) <- ss, mismatches sub s <= d ]
  where
    ss = zip [0..] $ chunks (length sub) str
    cond (_,s) = mismatches sub s <= d

count d sub str = length (patternIndices d sub str)

kmersWithCount d l dna = [ (kmer, count d kmer dna) | kmer <- genKmers ]
  where
    genKmers = sequence $ replicate l "ACGT"

mostFreqKmers d l dna = fst $ foldl go ([], 0) kwc
  where
    kwc = kmersWithCount d l dna
    go (kmers, n) (kmer, c)
        | n == c    = (kmer:kmers, n)
        | c > n     = ([kmer], c)
        | otherwise = (kmers, n)
-}

mostFreqKmers d l dna = [kmer | (kmer, cnt) <- kmers, cnt == maxim ]
  where
    kmers = M.toList $ allKmers d l dna
    maxim = snd $ maximumBy go kmers
      where
        go (_,c1) (_,c2) = c1 `compare` c2

allKmers d l dna = foldl (flip go) M.empty kmers
  where
    go kmer m = foldl (flip $ M.alter (Just . maybe 1 (+1))) m $ approximate d kmer
    kmers = chunks l dna

    approximate 0 kmer   = [kmer]
    approximate _ []     = [[]]
    approximate d (n:ns) = map (n:) (approximate d ns) ++ [ o:os | o <- other n, os <- approximate (d - 1) ns ]

    other 'A' = "CGT"
    other 'C' = "AGT"
    other 'G' = "ACT"
    other 'T' = "ACG"

main = do
    [dna, l, d] <- fmap words getLine
    putStrLn . intercalate " " $ mostFreqKmers (read d) (read l) dna
