import Data.List

main = do
    letters <- fmap (map head . words) getLine
    scores <- fmap (concat . map (readLine letters) . lines) getContents
    print scores
  where
    readLine :: [Char] -> String -> [((Char,Char),Int)]
    readLine ls s = zipWith (\t v -> ((f,t),read v)) ls ss
      where
        (fs:ss) = words s
        f = head fs
