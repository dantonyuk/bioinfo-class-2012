import Data.List

chunks l = takeWhile ((l==) . length) . map (take l) . tails

hamming pat str = length . filter not $ zipWith (==) pat str

patterns k = sequence $ replicate k "ATGC"

dist k pat dnas = sum $ map (minimum . map (hamming pat) . chunks k) dnas

bestPattern k dnas = minimumBy cmpDist $ patterns k
  where
    cmpDist pat1 pat2 = dist k pat1 dnas `compare` dist k pat2 dnas

main = do
    k <- fmap read getLine
    dnas <- fmap lines getContents
    putStrLn $ bestPattern k dnas
