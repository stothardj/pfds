{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module RanklessBinomialHeap where

import Data.Maybe
import Heap

-- 3.6

data BinomialTree k = BinomialTree k [BinomialTree k]
type BinomialHeapElem k = (Int, BinomialTree k)
type BinomialHeap k = [BinomialHeapElem k]

empty :: BinomialHeap k
empty = []

isEmpty :: BinomialHeap k -> Bool
isEmpty ts = null ts

root :: BinomialTree t -> t
root (BinomialTree rt _) = rt

elemRoot :: BinomialHeapElem t -> t
elemRoot = root . snd

link :: Ord k => BinomialTree k -> BinomialTree k -> BinomialTree k
link t1@(BinomialTree x1 c1) t2@(BinomialTree x2 c2)
  | x1 < x2 = BinomialTree x1 (t2 : c1)
  | otherwise =  BinomialTree x2 (t1 : c2)

insTree :: Ord k => BinomialHeapElem k -> BinomialHeap k -> BinomialHeap k
insTree x [] = [x]
insTree x@(r, t) ts@((r', t') : ts')
  | r < r' = x : ts
  | otherwise = insTree (r, (link t t')) ts'

singleton :: k -> BinomialTree k
singleton e = BinomialTree e []

insert :: Ord k => k -> BinomialHeap k -> BinomialHeap k
insert x ts = insTree (0, (singleton x)) ts

merge :: Ord t => BinomialHeap t -> BinomialHeap t -> BinomialHeap t
merge h [] = h
merge [] h = h
merge ts1@(x1@(r1, t1) : ts1') ts2@(x2@(r2, t2) : ts2')
  | r1 < r2 = x1 : RanklessBinomialHeap.merge ts1' ts2
  | r1 > r2 = x2 : RanklessBinomialHeap.merge ts1 ts2'               
  | otherwise = insTree (r1, (link t1 t2)) (RanklessBinomialHeap.merge  ts1' ts2')

removeMinTree :: Ord a => BinomialHeap a -> Maybe (BinomialHeapElem a, BinomialHeap a)
removeMinTree [] = Nothing
removeMinTree [t] = Just (t, [])
removeMinTree (t:ts)
  | elemRoot t < elemRoot t' = Just (t, ts)
  | otherwise = Just (t', t:ts')
  where (t', ts') = fromJust $ removeMinTree ts

findMin :: Ord c => BinomialHeap c -> c
findMin = elemRoot . fst . fromJust . removeMinTree

deleteMin :: Ord t => BinomialHeap t -> BinomialHeap t
deleteMin ts = RanklessBinomialHeap.merge (zip [0..] $ reverse ts1) ts2
  where ((_, (BinomialTree _ ts1)), ts2) = fromJust $ removeMinTree ts

instance (Ord k) => Heap (BinomialHeap k) k where
  empty = RanklessBinomialHeap.empty
  isEmpty = RanklessBinomialHeap.isEmpty
  insert = RanklessBinomialHeap.insert
  merge = RanklessBinomialHeap.merge
  findMin = RanklessBinomialHeap.findMin
  deleteMin = RanklessBinomialHeap.deleteMin
