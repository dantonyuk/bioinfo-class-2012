{-# LANGUAGE BangPatterns #-}

import Control.Monad
import Data.List
import qualified Data.Set as S

import Debug.Trace

subs xs = [[0]] ++ ([1..len-1] >>= parts) ++ [xs]
  where
    len = length xs
    parts l = take len . map (take l) $ tails (xs++xs)

cycloSpectrum :: [Int] -> [Int]
cycloSpectrum = map sum . subs

scoreWith = go 0
  where
    go !n [] _ = n
    go n _ [] = n
    go n (x:xs) (y:ys)
        | x < y  = go n xs (y:ys)
        | x > y  = go n (x:xs) ys
        | x == y = go (n+1) xs ys

convolute spectrum = filter (/=0) . concat . zipWith go (init spectrum) . tail $ tails spectrum
  where
    go x = map (\y -> abs (y-x))

cycloSequencing m n spectrum = trace (show acidMasses) $ go [[]] ([],0)
  where
    grouped = map f . group . takeWhile (<=200) . dropWhile (<57) . sort $ convolute spectrum
      where
        f xs@(x:_) = (x, length xs)

    acidMasses :: [Int]
    acidMasses = map fst . f . sortBy (\(_,x) (_,y) -> y `compare` x) $ grouped
      where
        f xs = ls ++ takeWhile ((r==) . snd) rs
          where
            (ls, rs@((_,r):_)) = splitAt (m - 1) xs

    expand = liftM2 (:) acidMasses

    parentMass = last spectrum

    score = scoreWith spectrum . sort . cycloSpectrum

    isFinal (_, s) = s == parentMass

    go []   (l,s) = l
    go list (l,s) = trace ("*") $ go (map (fst . fst) get) (lp1,ls1)
      where
        cs = filter consistentE $ map (\p -> (p, sum p)) $ expand list
        consistentE (_,s) = s <= parentMass
        os = sortBy (\(_,x) (_,y) -> y `compare` x) $ map (\(p, s) -> ((p, s), score p)) $ cs

        ((lp,_),ls) = maximumBy (\(_,x) (_,y) -> x `compare` y) $ ((l,0),s) : filter (isFinal . fst) os
        (lp1,ls1) = if s == ls then (l,s) else (lp,ls)

        has 0 _ = True
        has n [] = False
        has n (x:xs) = has (n-1) xs

        get | not (has (n+1) os) = os
            | otherwise          = ls ++ takeWhile (\(_,l) -> l == r) rs
          where
            (ls,(_,r):rs) = splitAt (n-1) os

main = do
    m <- fmap read getLine
    n <- fmap read getLine
    spectrum <- fmap (sort . map read . words) getLine
    let cs = cycloSequencing m n spectrum
    putStrLn . intercalate "-" . map show $ cs
