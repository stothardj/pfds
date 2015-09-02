{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module LeftistHeap where

import Data.Maybe
import qualified Data.Sequence as Seq
import Data.Sequence (Seq, (|>))
import Heap

-- Naive implementation of leftist heap (given)

-- Tree is rank, root, left subtree, right subtree where rank
-- is defined as the length of the right spline.
data LeftistHeap k = Empty | Tree Int k (LeftistHeap k) (LeftistHeap k)
                   deriving Show

rank :: LeftistHeap t -> Int
rank Empty = 0
rank (Tree r _ _ _) = r

makeT :: t -> LeftistHeap t -> LeftistHeap t -> LeftistHeap t
makeT root suba subb
  | ranka >= rankb = Tree (rankb + 1) root suba subb
  | otherwise = Tree (ranka + 1) root subb suba
  where ranka = rank suba
        rankb = rank subb

empty :: LeftistHeap k
empty = Empty

isEmpty :: LeftistHeap t -> Bool
isEmpty Empty = True
isEmpty _ = False

merge :: Ord a => LeftistHeap a -> LeftistHeap a -> LeftistHeap a
merge h Empty = h
merge Empty h = h
merge h1@(Tree _ x a1 b1) h2@(Tree _ y a2 b2)
  | x < y = makeT x a1 (LeftistHeap.merge b1 h2)
  | otherwise = makeT y a2 (LeftistHeap.merge h1 b2)

insert :: Ord a => a -> LeftistHeap a -> LeftistHeap a
insert e h = LeftistHeap.merge (singleton e) h

findMin :: LeftistHeap a -> Maybe a
findMin Empty = Nothing
findMin (Tree _ x _ _) = Just x

deleteMin :: Ord a => LeftistHeap a -> Maybe (LeftistHeap a)
deleteMin Empty = Nothing
deleteMin (Tree _ _ a b) = Just $ LeftistHeap.merge a b

-- Exercise 3.2: Define insert directly instead of using merge

insert2 :: Ord a => a -> LeftistHeap a -> LeftistHeap a
insert2 e Empty = Tree 1 e Empty Empty
insert2 e h@(Tree r x a b)
  | e < x = Tree (r+1) e Empty h
  | otherwise = makeT x a (insert2 e b)

-- Excerise 3.3: Define fromList. Should not do a single pass of foldl, but should
-- instead group pairs of lists and merge
-- Note I cheat a bit and use a built in queue

fromSeq :: Ord k => Seq (LeftistHeap k) -> LeftistHeap k
fromSeq heaps
  | Seq.null heaps = Empty
  | Seq.length heaps == 1 = h1
  | otherwise = fromSeq (hs |> mh)
  where h1 = Seq.index heaps 0
        h2 = Seq.index heaps 1
        mh = LeftistHeap.merge h1 h2
        hs = Seq.drop 2 heaps

singleton :: k -> LeftistHeap k
singleton e = Tree 1 e Empty Empty

fromList :: Ord k => [k] -> LeftistHeap k
fromList xs = fromSeq (Seq.fromList heaps)
  where heaps = map singleton xs

instance (Ord k) => Heap (LeftistHeap k) k where
  empty = LeftistHeap.empty
  isEmpty = LeftistHeap.isEmpty
  insert = LeftistHeap.insert
  merge = LeftistHeap.merge
  findMin = fromJust . LeftistHeap.findMin
  deleteMin = fromJust . LeftistHeap.deleteMin
