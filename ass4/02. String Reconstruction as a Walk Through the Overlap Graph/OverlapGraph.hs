import Data.List
import Data.Monoid
import qualified Data.Map as M

toGraph kmers = edges
  where
    prefixes = foldr go M.empty kmers
      where
        go kmer = M.alter (Just . maybe [kmer] (kmer:)) (init kmer)
    edges = foldr go M.empty kmers
      where
        go kmer = M.insert kmer (M.findWithDefault [] (tail kmer) prefixes)

main = do
    kmers <- fmap lines getContents
    mapM_ go $ M.toList (toGraph kmers)
  where
    go (l,rs) = mapM_ (\r -> putStrLn $ l ++ " -> " ++ r) rs
