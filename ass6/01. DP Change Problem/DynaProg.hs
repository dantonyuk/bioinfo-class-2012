import Data.List
import Data.List.Split
import Data.Maybe
import qualified Data.Map as M


dynaProg coins money = go 0 M.empty
  where
    go 0 _ = go 1 $ M.singleton 0 0
    go m cache
        | m == money + 1 = get money
        | otherwise      = go (m+1) $ M.insert m num cache
      where
        get key = fromJust $ M.lookup key cache
        num = 1 + minimum [get (m - coin) | coin <- coins, coin <= m]

main = do
    money <- fmap read getLine
    coins <- fmap readCoins getLine
    print $ dynaProg coins money
  where
    readCoins = map read . splitOn ","
