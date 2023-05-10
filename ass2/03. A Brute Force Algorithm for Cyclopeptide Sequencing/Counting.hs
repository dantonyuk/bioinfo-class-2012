import Control.Monad

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

--acidMasses = [57, 71, 87, 97, 99, 101, 103, 113, 114, 115, 128, 129, 131, 137, 147, 156, 163, 186]
acidMasses = [57, 71, 87, 97, 99, 101, 103, 113, 113, 114, 115, 128, 128, 129, 131, 137, 147, 156, 163, 186]

peptideMasses = go [[]]
  where
    go xs = let ys = masses xs in ys ++ go ys
      where
       masses = liftM2 (:) acidMasses

minMass = head acidMasses

splitMass :: Int -> [[Int]]
splitMass = go [[]]
  where
    go :: [[Int]] -> Int -> [[Int]]
    go xs 0 = xs
    go xs mass = concat $ liftM2 add acidMasses xs
      where
        add :: Int -> [Int] -> [[Int]]
        add acidMass masses
            | acidMass < mass = map (acidMass:) $ splitMass (mass-acidMass)
            | acidMass > mass = []
            | otherwise       = [mass:masses]

count = foldr addCoin (1:repeat 0)
  where
    addCoin c oldlist = newlist
      where
        newlist = (take c oldlist) ++ zipWith (+) newlist (drop c oldlist)

main3 = do
    mass <- fmap read getLine
    print (count acidMasses !! mass)

main = do
    mass <- fmap read getLine
    print . length $ splitMass mass

main2 = do
    mass <- fmap read getLine
    let maxNo = mass `div` minMass
        suitable = filter ((==mass) . sum) $ takeWhile ((<=maxNo) . length) peptideMasses
    print $ length suitable
