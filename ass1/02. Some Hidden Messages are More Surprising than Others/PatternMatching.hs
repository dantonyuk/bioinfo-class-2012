import Data.List

main = do
    substr <- getLine
    str <- getLine
    putStrLn $ intercalate " " $ map (show . fst) $ filter ((substr==) . snd) $ zip [0..] $ map (take $ length substr) $ tails str
