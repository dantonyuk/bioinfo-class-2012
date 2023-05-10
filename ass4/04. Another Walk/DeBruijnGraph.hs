import Data.List
import Data.Monoid
import qualified Data.Map as M

chunks l = takeWhile ((l==) . length) . map (take l) . tails

toGraph kmers = edges
  where
    adjacents = map go kmers
      where
        go kmer = (init kmer, tail kmer)

    edges = foldr go M.empty adjacents
      where
        go (l,r) = M.alter (Just . maybe [r] (r:)) l

main = do
    kmers <- fmap lines getContents
    mapM_ go $ M.toList (toGraph kmers)
  where
    go (l,rs) = putStrLn $ l ++ " -> " ++ intercalate "," (sort rs)
