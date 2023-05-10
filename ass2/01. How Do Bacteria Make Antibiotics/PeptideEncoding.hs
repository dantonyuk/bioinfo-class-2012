import Data.List
import Data.List.Split
import Data.Maybe

translateCodon "AAA" = "K"
translateCodon "AAC" = "N"
translateCodon "AAG" = "K"
translateCodon "AAU" = "N"
translateCodon "ACA" = "T"
translateCodon "ACC" = "T"
translateCodon "ACG" = "T"
translateCodon "ACU" = "T"
translateCodon "AGA" = "R"
translateCodon "AGC" = "S"
translateCodon "AGG" = "R"
translateCodon "AGU" = "S"
translateCodon "AUA" = "I"
translateCodon "AUC" = "I"
translateCodon "AUG" = "M"
translateCodon "AUU" = "I"
translateCodon "CAA" = "Q"
translateCodon "CAC" = "H"
translateCodon "CAG" = "Q"
translateCodon "CAU" = "H"
translateCodon "CCA" = "P"
translateCodon "CCC" = "P"
translateCodon "CCG" = "P"
translateCodon "CCU" = "P"
translateCodon "CGA" = "R"
translateCodon "CGC" = "R"
translateCodon "CGG" = "R"
translateCodon "CGU" = "R"
translateCodon "CUA" = "L"
translateCodon "CUC" = "L"
translateCodon "CUG" = "L"
translateCodon "CUU" = "L"
translateCodon "GAA" = "E"
translateCodon "GAC" = "D"
translateCodon "GAG" = "E"
translateCodon "GAU" = "D"
translateCodon "GCA" = "A"
translateCodon "GCC" = "A"
translateCodon "GCG" = "A"
translateCodon "GCU" = "A"
translateCodon "GGA" = "G"
translateCodon "GGC" = "G"
translateCodon "GGG" = "G"
translateCodon "GGU" = "G"
translateCodon "GUA" = "V"
translateCodon "GUC" = "V"
translateCodon "GUG" = "V"
translateCodon "GUU" = "V"
translateCodon "UAA" = ""
translateCodon "UAC" = "Y"
translateCodon "UAG" = ""
translateCodon "UAU" = "Y"
translateCodon "UCA" = "S"
translateCodon "UCC" = "S"
translateCodon "UCG" = "S"
translateCodon "UCU" = "S"
translateCodon "UGA" = ""
translateCodon "UGC" = "C"
translateCodon "UGG" = "W"
translateCodon "UGU" = "C"
translateCodon "UUA" = "L"
translateCodon "UUC" = "F"
translateCodon "UUG" = "L"
translateCodon "UUU" = "F"
translateCodon x = ""

complement = map go
  where
    go 'A' = 'U'
    go 'U' = 'A'
    go 'C' = 'G'
    go 'G' = 'C'

transcribe :: String -> String
transcribe = map go
  where
    go 'T' = 'U'
    go x = x

translate :: String -> String
translate = concatMap translateCodon . chunksOf 3

match :: String -> String -> [String]
match peptide rna = mapMaybe (go . take (len * 3)) $ tails rna
  where
    len = length peptide
    go strand | aminos1 == peptide = Just strand
              | aminos2 == peptide = Just strand
              | otherwise          = Nothing
      where
        aminos1 = translate strand
        aminos2 = translate . reverse $ complement strand

toDNA :: String -> String
toDNA = map go
  where
    go 'U' = 'T'
    go x = x

main = do
    dna <- getLine
    peptide <- getLine
    let rna = transcribe dna
        matched = match peptide rna
    mapM_ (putStrLn . toDNA) matched
