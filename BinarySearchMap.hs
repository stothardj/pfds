module BinarySearchMap where

import Prelude hiding (lookup)

-- 2.6: Create a finite map using the ideas from the set above
-- Using the naive approaches of the methods for simplicity

data BSTMap k v = BSTMapEmpty | BSTMapTree (BSTMap k v) (k,v) (BSTMap k v)
                deriving Show

empty :: BSTMap k v
empty = BSTMapEmpty

bind :: Ord a => a -> v -> BSTMap a v -> BSTMap a v
bind k v BSTMapEmpty = BSTMapTree empty (k,v) empty
bind k v m@(BSTMapTree l p@(ck,_) r)
  | k < ck = BSTMapTree (bind k v l) p r
  | k > ck = BSTMapTree l p (bind k v r)
  | otherwise = m

lookup :: Ord a1 => a1 -> BSTMap a1 a -> Maybe a
lookup _ BSTMapEmpty = Nothing
lookup k (BSTMapTree l (ck,cv) r)
  | k < ck = lookup k l
  | k > ck = lookup k r
  | otherwise = Just cv
