complement = map go
  where
    go 'A' = 'T'
    go 'T' = 'A'
    go 'G' = 'C'
    go 'C' = 'G'


main = getLine >>= putStrLn . reverse . complement
