import Data.List
import Data.Monoid
import qualified Data.Map as M

chunks l = takeWhile ((l==) . length) . map (take l) . tails

toGraph k dna = edges
  where
    adjacents = zip kmers (tail kmers)
      where
        kmers = chunks k dna

    edges = foldr go M.empty adjacents
      where
        go (l,r) = M.alter (Just . maybe [r] (r:)) l

main = do
    k <- fmap read getLine
    dna <- getLine
    mapM_ go $ M.toList (toGraph (k-1) dna)
  where
    go (l,rs) = putStrLn $ l ++ " -> " ++ intercalate "," (sort rs)
