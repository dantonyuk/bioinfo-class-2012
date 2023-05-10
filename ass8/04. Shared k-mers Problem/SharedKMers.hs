import Control.Monad
import Data.List
import Data.List.Split
import Data.Maybe
import qualified Data.Map as M

complement 'A' = 'T'
complement 'T' = 'A'
complement 'C' = 'G'
complement 'G' = 'C'

chunks l = takeWhile ((l==) . length) . map (take l) . tails

shared k s1 s2 = do
    (i2,c2) <- cs2
    let result@(~(Just is1)) = M.lookup c2 cache
    guard $ isJust result
    i1 <- reverse is1
    return (i1,i2)
  where
    mkIndexedChunks = zip [0..] . chunks k

    cs1 = mkIndexedChunks s1
    cs2 = mkIndexedChunks s2

    cache = foldl' (flip go) M.empty cs1
      where
        go (i,c)
            | c == c_rev = update i c
            | otherwise  = update i c . update i c_rev
          where
            c_rev = map complement $ reverse c
        update i = M.alter (Just . maybe [i] (i:))

    check c1 c2 = c1 == c2 || c1 == map complement c2

main = do
    k <- fmap read getLine
    s1 <- getLine
    s2 <- getLine
    mapM_ (putStrLn . showIdx) $ shared k s1 s2
  where
    showIdx (x,y) = "(" ++ show x ++ ", " ++ show y ++ ")"