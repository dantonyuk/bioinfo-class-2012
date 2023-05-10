import Data.List
import qualified Data.Map as M

chunks l = takeWhile ((l==) . length) . map (take l) . tails

findClumps k l t dna = suitable
  where
    inc (i, key) = M.alter (Just . maybe [i] updateIndices) key
      where
        updateIndices is | length is >= t = is
                         | otherwise   = i : takeWhile inWindow is
        inWindow j = i - j + k <= l

    kmers = foldl (flip inc) M.empty $ zip [0..] $ chunks k dna
    suitable = M.keys $ M.filter ((>=t) . length) kmers

main = do
    dna <- getLine
    [k,l,t] <- (map read . words) `fmap` getLine
    putStrLn $ intercalate " " $ findClumps k l t dna
