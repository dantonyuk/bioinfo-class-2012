import Trie

main = do
    trie <- fmap (foldr insert empty . lines) getContents
    enumerate trie >>= showEdges