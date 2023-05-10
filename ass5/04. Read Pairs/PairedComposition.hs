import Data.List

chunks l = takeWhile ((l==) . length) . map (take l) . tails

dna = "TAATGCCATGGGATGTT"

compositions k d = sort . map pair . chunks (2*k+d)
  where
    pair read = (take k read, drop (k + d) read)

main = putStrLn . showComp $ compositions 3 2 dna
  where
    showComp = intercalate "," . map showPair
    showPair (a,b) = "(" ++ a ++ "|" ++ b ++ ")"
