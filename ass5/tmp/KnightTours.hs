{-# LANGUAGE TupleSections #-}

import Data.List

links = [
    (1,[2,6,8]),
    (2,[3,5]),
    (3,[4,8]),
    (4,[5,7]),
    (5,[6]),
    (6,[7]),
    (7,[8])]

edges = links >>= (\(x,xs) -> map (x,) xs)

deleteWith n = filter without
  where
    without (l,r) = not (n `elem` [l,r])

path 1 [] es = concatMap (\(1,n) -> path n [[1]] (delete (1,n) es)) ns
  where
    ns = filter ((1==) . fst) es
path 1 ps es = ps
path from ps [] = []
path from ps es = concatMap (\e -> path (to e) (map (from:) ps) (deleteWith from es)) ns
  where
    to (l,r) | l == from = r
             | True      = l
    ns = filter withFrom es
    withFrom (l,r) = from `elem` [l,r]

pathes = path 1 [] edges
