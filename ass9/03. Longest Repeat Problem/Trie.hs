{-# LANGUAGE ViewPatterns #-}

module Trie where

--import Control.Applicative
import Control.Monad
import Data.IORef
import Data.List (tails)
import Data.Maybe
import qualified Data.List as L
import qualified Data.Map as M

data Trie a = Trie (Maybe Int) (M.Map a (Trie a))
    deriving Show

empty :: Trie a
empty = Trie Nothing M.empty

null :: Trie a -> Bool
null (Trie _ tries) = M.null tries

insert :: Ord a => Int -> [a] -> Trie a -> Trie a
insert n [] (Trie Nothing tries) = Trie (Just n) tries
insert n (x:xs) (Trie m tries) = Trie m $ M.alter f x tries
  where
    f = Just . insert n xs . fromMaybe empty

fmapTrie f (Trie n tries) = Trie n $ fmap (fmapTrie f) $ M.mapKeys f tries

prefixes :: Ord a => Trie a -> [a] -> [Int]
prefixes trie [] = catMaybes $ indices trie
  where
    indices (Trie n tries) = n : (M.elems tries >>= indices)
prefixes (Trie _ tries) (x:xs)
    | Just trie <- M.lookup x tries = prefixes trie xs
    | otherwise                     = []

match :: Ord a => Trie a -> [[a]] -> [Int]
match trie = (>>= prefixes trie)
