import Data.List

findKMers k dna = map head $ head $ groupBy groupF $ sortBy sortF $ group $ sort klets
  where
    klets = [s | t <- tails dna, let s = take k t, length s == k]
    sortF x y = length y `compare` length x
    groupF x y = length x == length y

main = do
    dna <- getLine
    k <- fmap read getLine
    putStrLn $ intercalate " " $ findKMers k dna
