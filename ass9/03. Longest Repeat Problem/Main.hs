import Data.List (intercalate, tails, init)

import Trie

main = do
    text <- getLine
    let trie = foldr (uncurry insert) empty . zip [0..] $ tails text
    putStrLn $ longestRepeat trie
