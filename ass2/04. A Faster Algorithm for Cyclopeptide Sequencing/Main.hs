import Control.Monad
import Data.List

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

acidMasses = [57, 71, 87, 97, 99, 101, 103, 113, 114, 115, 128, 129, 131, 137, 147, 156, 163, 186]
--acidMasses = [57, 71, 87, 97, 99, 101, 103, 113, 113, 114, 115, 128, 128, 129, 131, 137, 147, 156, 163, 186]

acids = "GASPVTCILNSKQEMHFRYW"

expand = liftM2 (:) acidMasses

subs xs = [[0]] ++ ([1..len-1] >>= parts) ++ [xs]
  where
    len = length xs
    parts l = take len . map (take l) $ tails (xs++xs)

cycloSpectrum :: [Int] -> [Int]
cycloSpectrum = map sum . subs

linearSpectrum :: [Int] -> [Int]
linearSpectrum xs = map sum $ tails xs >>= inits

consistent2 spectrum peptide = all (`elem` spectrum) . map sum $ inits peptide

consistent spectrum peptide = all (`elem` spectrum) peptideSpectrum
  where
    peptideSpectrum = linearSpectrum peptide


cycloSequencing spectrum = go [[]]
  where
    spec = nub $ sort spectrum
    isSpec p = spec == (nub $ sort $ cycloSpectrum p)
    go []   = []
    go list = fs ++ go os
      where
        ls = expand list
        cs = filter (consistent spec) ls
        (fs, os) = partition isSpec cs

main = do
    spectrum <- fmap (map read . words) getLine
    let cs = cycloSequencing spectrum
    putStrLn . intercalate " " $ map (intercalate "-" . map show) cs
