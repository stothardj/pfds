{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module BinomialHeap where

import Data.Maybe
import Heap

data BinomialTree k = BinomialTree Int k [BinomialTree k]
type BinomialHeap k = [BinomialTree k]

empty :: BinomialHeap k
empty = []

isEmpty :: BinomialHeap t -> Bool
isEmpty ts = null ts

rank :: BinomialTree t -> Int
rank (BinomialTree r _ _) = r

root :: BinomialTree t -> t
root (BinomialTree _ rt _) = rt

link :: Ord k => BinomialTree k -> BinomialTree k -> BinomialTree k
link t1@(BinomialTree r x1 c1) t2@(BinomialTree _ x2 c2)
  | x1 < x2 = BinomialTree (r+1) x1 (t2 : c1)
  | otherwise = BinomialTree (r+1) x2 (t1 : c2)

insTree
  :: Ord t => BinomialTree t -> BinomialHeap t -> BinomialHeap t
insTree t [] = [t]
insTree t ts@(t' : ts')
  | rank t < rank t' = t : ts
  | otherwise = insTree (link t t') ts'

singleton :: k -> BinomialTree k
singleton e = BinomialTree 0 e []

insert :: Ord t => t -> BinomialHeap t -> BinomialHeap t
insert x ts = insTree (singleton x) ts

merge :: Ord t => BinomialHeap t -> BinomialHeap t -> BinomialHeap t
merge ts [] = ts
merge [] ts = ts
merge ts1@(t1 : ts1') ts2@(t2 : ts2')
  | r1 < r2 = t1 : BinomialHeap.merge ts1' ts2
  | r2 > r1 = t2 : BinomialHeap.merge ts1 ts2'
  | otherwise = insTree (link t1 t2) (BinomialHeap.merge ts1' ts2')
  where r1 = rank t1
        r2 = rank t2

removeMinTree
  :: Ord a =>
     [BinomialTree a] -> Maybe (BinomialTree a, [BinomialTree a])
removeMinTree [] = Nothing
removeMinTree [t] = Just (t, [])
removeMinTree (t:ts)
  | root t < root t' = Just (t, ts)
  | otherwise = Just (t', t:ts')
  where (t', ts') = fromJust $ removeMinTree ts

findMin :: Ord c => BinomialHeap c -> c
findMin = root . fst . fromJust . removeMinTree

deleteMin :: Ord c => BinomialHeap c -> BinomialHeap c  
deleteMin ts = BinomialHeap.merge (reverse ts1) ts2
  where ((BinomialTree _ _ ts1), ts2) = fromJust $ removeMinTree ts

-- 3.5
findMin2 :: Ord t => BinomialHeap t -> t
findMin2 [t] = root t
findMin2 (t:ts) = min (root t) (findMin2 ts)

instance (Ord k) => Heap (BinomialHeap k) k where
  empty = BinomialHeap.empty
  isEmpty = BinomialHeap.isEmpty
  insert = BinomialHeap.insert
  merge = BinomialHeap.merge
  findMin = BinomialHeap.findMin
  deleteMin = BinomialHeap.deleteMin
