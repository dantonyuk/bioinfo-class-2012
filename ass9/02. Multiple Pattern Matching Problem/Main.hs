import Data.List (intercalate)

import Trie

main = do
    text <- getLine
    trie <- fmap (foldr insert empty . lines) getContents
    putStrLn . intercalate " " . map show $ match trie text