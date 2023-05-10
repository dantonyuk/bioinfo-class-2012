import Data.List

chunks l = takeWhile ((l==) . length) . map (take l) . tails

genSame 0 kmer = [kmer]
genSame _ [] = [[]]
genSame d (x:xs) = map (x:) (genSame d xs) ++ [ y:ys | y <- other x, ys <- genSame (d-1) xs]
  where
   other 'A' = "TGC"
   other 'T' = "AGC"
   other 'G' = "ATC"
   other 'C' = "ATG"

motifEnumeration dnas k d = [ samekmer | kmer <- allkmers, samekmer <- genSame d kmer, eachHasSame samekmer ]
  where
    kmers = chunks k
    allkmers = dnas >>= kmers
    same kmer1 kmer2 = d >= length (filter not $ zipWith (==) kmer1 kmer2)
    eachHasSame kmer = and $ map (not . null . filter (same kmer) . kmers) dnas

main = do
    [k, d] <- fmap (map read . words) getLine
    dnas <- fmap lines getContents
    putStrLn . intercalate " " . nub $ motifEnumeration dnas k d
