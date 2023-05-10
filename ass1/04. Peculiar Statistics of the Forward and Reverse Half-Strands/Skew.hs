import Data.List

skew :: String -> [Int]
skew = scanl (flip go) 0
  where
    go 'C' = pred
    go 'G' = succ
    go _ = id

findMinIndicesInSkew dna = [ i | (i, v) <- zip [0..] s, v == m ]
  where
    s = skew dna
    m = minimum s

main = do
  dna <- getLine
  -- putStrLn $ intercalate " " $ map show $ skew dna
  putStrLn $ intercalate " " $ map show $ findMinIndicesInSkew dna
