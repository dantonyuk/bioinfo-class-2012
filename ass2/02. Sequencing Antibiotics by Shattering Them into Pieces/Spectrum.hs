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

subs xs = [""] ++ ([1..len-1] >>= parts) ++ [xs]
  where
    len = length xs
    parts l = take len . map (take l) $ tails (xs++xs)

main = getLine >>= putStrLn . intercalate " " . map show . sort . map (sum . map mass) . subs
