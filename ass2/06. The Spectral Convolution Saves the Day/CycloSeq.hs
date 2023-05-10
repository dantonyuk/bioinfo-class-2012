{-# LANGUAGE BangPatterns #-}

import Control.Monad
import Data.List
import qualified Data.Set as S

import Debug.Trace

{-
mass 'G' = 57
mass 'A' = 71
mass 'S' = 87
mass 'P' = 97
mass 'V' = 99
mass 'T' = 101
mass 'C' = 103
mass 'I' = 113
mass 'L' = 113
mass 'N' = 114
mass 'D' = 115
mass 'K' = 128
mass 'Q' = 128
mass 'E' = 129
mass 'M' = 131
mass 'H' = 137
mass 'F' = 147
mass 'R' = 156
mass 'Y' = 163
mass 'W' = 186

acidsByMass 57 = "G"
acidsByMass 71 = "A"
acidsByMass 87 = "S"
acidsByMass 97 = "P"
acidsByMass 99 = "V"
acidsByMass 101 = "T"
acidsByMass 103 = "C"
acidsByMass 113 = "IL"
acidsByMass 114 = "N"
acidsByMass 115 = "D"
acidsByMass 128 = "KQ"
acidsByMass 129 = "E"
acidsByMass 131 = "M"
acidsByMass 137 = "H"
acidsByMass 147 = "F"
acidsByMass 156 = "R"
acidsByMass 163 = "Y"
acidsByMass 186 = "W"
-}

acidMasses :: [Int]
--acidMasses = [57, 71, 87, 97, 99, 101, 103, 113, 114, 115, 128, 129, 131, 137, 147, 156, 163, 186]
acidMasses = [99,71,147,97,186,101,128,198,170,168,126,196,69,90,113,78,92,57,85,87,114,179]
--acidMasses = [57, 71, 87, 97, 99, 101, 103, 113, 113, 114, 115, 128, 128, 129, 131, 137, 147, 156, 163, 186]

--acids = "GASPVTCILNSKQEMHFRYW"

expand :: [[Int]] -> [[Int]]
expand = liftM2 (:) acidMasses

subs xs = [[0]] ++ ([1..len-1] >>= parts) ++ [xs]
  where
    len = length xs
    parts l = take len . map (take l) $ tails (xs++xs)

cycloSpectrum :: [Int] -> [Int]
cycloSpectrum = map sum . subs

-- linearSpectrum :: [Int] -> [Int]
-- linearSpectrum xs = map sum $ init (tails xs) >>= tail . inits

scoreWith = go 0
  where
    go !n [] _ = n
    go n _ [] = n
    go n (x:xs) (y:ys)
        | x < y  = go n xs (y:ys)
        | x > y  = go n (x:xs) ys
        | x == y = go (n+1) xs ys

cycloSequencing n spectrum = go [[]] ([],0)
  where
    parentMass = last spectrum
    specMap = S.fromList spectrum
--    scoreBy f = scoreWith spectrum . sort . f
--    scoreBy f = length . filter (`S.member` specMap) . nub . f
--    scoreBy f peptide = S.size (specMap `S.intersection` S.fromList (f peptide))

--    lscore = scoreBy linearSpectrum
--    cscore = scoreBy cycloSpectrum

    score = scoreWith spectrum . sort . cycloSpectrum

    isFinal (_, s) = s == parentMass

    go []   (l,s) = l
    --go list (l,s) = trace (show get ++ "\n") $ go (map (fst . fst) get) (lp1,ls1)
    go list (l,s) = go (map (fst . fst) get) (lp1,ls1)
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
    n <- fmap read getLine
    spectrum <- fmap (map read . words) getLine
    let cs = cycloSequencing n spectrum
    putStrLn . intercalate "-" . map show $ cs
