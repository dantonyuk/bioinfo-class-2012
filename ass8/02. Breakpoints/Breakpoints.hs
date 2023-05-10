import Data.List

breakpoints :: [Int] -> Int
breakpoints p = go ((0:p) ++ [length p + 1])
  where
    go ls = sum $ zipWith f ls (tail ls)
    f x y | y - x == 1 = 0
          | otherwise  = 1

main = do
    ps <- fmap (map read . words . filter (`notElem` "+()")) getLine
    print $ breakpoints ps
