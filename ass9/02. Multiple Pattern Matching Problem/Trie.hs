{-# LANGUAGE ViewPatterns #-}

module Trie where

--import Control.Applicative
import Control.Monad
import Data.IORef
import Data.List (tails)
import Data.Maybe
import qualified Data.List as L
import qualified Data.Map as M

data Trie a = Trie (M.Map a (Trie a))
    deriving Show

empty :: Trie a
empty = Trie M.empty

null :: Trie a -> Bool
null (Trie tries) = M.null tries

insert :: Ord a => [a] -> Trie a -> Trie a
insert [] trie = trie
insert (x:xs) (Trie tries) = Trie $ M.alter f x tries
  where
    f = Just . insert xs . fromMaybe empty

fmapTrie f (Trie tries) = Trie $ fmap (fmapTrie f) $ M.mapKeys f tries

hasPrefix :: Ord a => Trie a -> [a] -> Bool
hasPrefix (Trie tries) [] = M.null tries
hasPrefix (Trie tries) text@(~(x:xs))
    | M.null tries                  = True
    | L.null text                     = False
    | Just trie <- M.lookup x tries = hasPrefix trie xs
    | otherwise                     = False

match :: Ord a=> Trie a -> [a] -> [Int]
match trie = mapMaybe go . zip [0..] . tails
  where
    go (i, text)
        | hasPrefix trie text = Just i
        | otherwise           = Nothing

enumerate trie = do
    counter <- newIORef 1
    go counter trie
  where
    go counter (Trie (M.toList -> tries)) = do
        fmap (Trie . M.fromList) . forM tries $ \(x,trie) -> do
            n <- readIORef counter
            modifyIORef counter succ
            newTrie <- go counter trie
            return ((n+1, x), newTrie)

showEdges trie = do
    t <- enumerate trie
    go 1 t
  where
    go n (Trie tries) = do
        forM_ (M.toList tries) $ \((i,(_,c)),trie) -> do
            putStrLn $ show n ++ " " ++ show i ++ " " ++ [c]
            go i trie