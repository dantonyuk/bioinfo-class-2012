import Data.List

chunks l = takeWhile ((l==) . length) . map (take l) . tails

composition k = sort . chunks k

main = do
    k <- fmap read getLine
    dna <- getLine
    mapM_ putStrLn $ composition k dna
