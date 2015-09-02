module BinarySearchTree where

import Data.Maybe

-- Binary search tree exercises

-- Naive implementations provided
data BST k = BSTEmpty | BSTTree (BST k) k (BST k)
           deriving Show

empty :: BST k
empty = BSTEmpty

member :: (Ord k) => k -> BST k -> Bool
member _ BSTEmpty = False
member e (BSTTree l c r)
  | e < c = member e l
  | e > c = member e r
  | otherwise = True

insert :: (Ord k) => k -> BST k -> BST k
insert e BSTEmpty = BSTTree empty e empty
insert e tree@(BSTTree l c r)
  | e < c = BSTTree (insert e l) c r
  | e > c = BSTTree l c (insert e r)
  | otherwise = tree

-- 2.2: Member with only d+1 wost case comparisons (where d is depth)

-- candidate is the last element for which e < c returned false.
-- This means that candidate might be equal to e, but we have not
-- checked yet.
-- candidate must be maybe in case e < c has never returned false
-- yet.
member2' :: (Ord k) => Maybe k -> k -> BST k -> Bool
member2' Nothing _ BSTEmpty = False
member2' (Just candidate) e BSTEmpty = e == candidate
member2' candidate e (BSTTree l c r)
  | e < c = member2' candidate e l
  | otherwise = member2' (Just c) e r

member2 :: (Ord k) => k -> BST k -> Bool
member2 = member2' Nothing

-- 2.3: Inserting an existing element should not modify the tree

-- the naive implementation copies nodes from the tree along the search path
-- as it goes. If it finds the node already existed, it at that point just shares
-- that subtree. Use an "exception" so that if the node already was in the tree
-- then just return that tree

-- Maybe is our exception model. We return Nothing if we find it was already there.
insert2' :: (Ord k) => k -> BST k -> Maybe (BST k)
insert2' e BSTEmpty = Just $ BSTTree empty e empty
insert2' e (BSTTree l c r)
  | e < c = do left <- insert2' e l
               return $ BSTTree left c r
  | e > c = do right <- insert2' e r
               return $ BSTTree l c right
  | otherwise = Nothing

insert2 :: (Ord k) => k -> BST k -> BST k
insert2 e tree = fromMaybe tree $ insert2' e tree

-- 2.4: No more than d+1 comparisons insert, and do not copy on inserting existing

insert3' :: (Ord k) => Maybe k -> k -> BST k -> Maybe (BST k)
insert3' Nothing e BSTEmpty = Just $ BSTTree empty e empty
insert3' (Just candidate) e BSTEmpty
  | candidate == e = Nothing
  | otherwise = Just $ BSTTree empty e empty
insert3' candidate e (BSTTree l c r)
  | e < c = do left <- insert3' candidate e l
               return $ BSTTree left c r
  | otherwise = do right <- insert3' (Just c) e r
                   return $ BSTTree l c right

insert3 :: (Ord k) => k -> BST k -> BST k
insert3 e tree = fromMaybe tree $ insert3' Nothing e tree

-- 2.5: Sharing within an object

-- a) Create a complete binary search tree given an element and a depth

completeTree :: (Eq a, Num a) => k -> a -> BST k
completeTree _ 0 = empty
completeTree e d = BSTTree t e t
  where t = completeTree e (d-1)

-- b) Create a binary tree of arbitrary size given an element and size, which
-- is as balanced as possible. Left and right subtrees can only differ by at most 1.

-- Creates a balanced tree of size m
balancedTree :: Integral a => k -> a -> BST k
balancedTree _ 0 = empty
balancedTree e m
  | odd m = BSTTree t1 e t1
  | even m = BSTTree t1 e t2
  where t1 = balancedTree e ((m-1) `div` 2)
        t2 = balancedTree e (((m-1) `div` 2) + 1)
