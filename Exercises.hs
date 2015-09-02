module Exercises where

-- 2.1: Suffixes
sx :: [a] -> [[a]]
sx [] = [[]]
sx ls@(_:xs) = ls:(sx xs)

-- 2.2-2.5: See BinarySearchTree.hs

-- 2.6: See BinarySearchMap.hs

-- 3.1: Prove that right spine of a leftmost heap of size n has contains at most
-- floor(log(n+1)) elements (log base 2)

-- The rightspin of the leftmost heap must be the shortest (or tied shortest) of the
-- branches.
-- Suppose it had more than floor(log(n+1)) nodes. But no other branch could be
-- shorter than it. So all branches would have more than log(n+1) nodes.
-- But since it's a binary tree that means there are more than 2^floor(log(n+1))
-- nodes. But floor(log(n+1)) >= log(n) (for all n we are considering) so there
-- are greater than 2^log(n), meaning greather than n nodes. Which is a contradition.


-- 3.2-: See LeftistHeap.hs
