import Data.List

chunks l = takeWhile ((l==) . length) . map (take l) . tails

mismatches t1 t2 = length . filter id $ zipWith (/=) t1 t2

patternIndices d sub str = [ i | (i,s) <- ss, mismatches sub s <= d ]
  where
    ss = zip [0..] $ chunks (length sub) str
    cond (_,s) = mismatches sub s <= d

main = do
    sub <- getLine
    str <- getLine
    d <- fmap read getLine
    putStrLn . intercalate " " . map show $ patternIndices d sub str
