{-# LANGUAGE PatternGuards #-}

import Control.Monad
import Control.Monad.State
import Data.List
import qualified Data.Map as M

recChange money coins = evalState (go money) M.empty
  where
    go 0     = return 0
    go money = do
        mv <- gets (M.lookup money)
        case mv of
            Just v -> return v
            Nothing -> do
                vs <- mapM rec $ filter (<=money) coins
                return (1 + minimum vs)
      where
        rec coin = do
            let newMoney = money - coin
            v <- go newMoney
            modify (M.insert newMoney v)
            return v

