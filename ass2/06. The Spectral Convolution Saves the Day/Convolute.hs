import Data.List

convolute spectrum = filter (/=0) . concat . zipWith go (init spectrum) . tail $ tails spectrum
  where
    go x = map (\y -> abs (y-x))

main = do
    spectrum <- fmap (map read . words) getLine
    putStrLn $ intercalate " " $ map show $ convolute spectrum
