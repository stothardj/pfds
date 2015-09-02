module WeightLeftistHeap where

-- Exercise 3.4

-- Tree is weight, root, left subtree, right subtree where weight is defined
-- as it's size.
data WeightLeftistHeap k = Empty | Tree Int k (WeightLeftistHeap k) (WeightLeftistHeap k)
                         deriving Show

weight :: WeightLeftistHeap t -> Int
weight Empty = 0
weight (Tree w _ _ _) = w

makeT :: t -> WeightLeftistHeap t -> WeightLeftistHeap t -> WeightLeftistHeap t
makeT root suba subb
  | weighta >= weightb = Tree total root suba subb
  | otherwise = Tree total root subb suba
  where weighta = weight suba
        weightb = weight subb
        total = 1 + weighta + weightb

empty :: WeightLeftistHeap k
empty = Empty

isEmpty :: WeightLeftistHeap t -> Bool
isEmpty Empty = True
isEmpty _ = False

merge :: Ord a => WeightLeftistHeap a -> WeightLeftistHeap a -> WeightLeftistHeap a
merge h Empty = h
merge Empty h = h
merge h1@(Tree _ x a1 b1) h2@(Tree _ y a2 b2)
  | x < y = makeT x a1 (merge b1 h2)
  | otherwise = makeT y a2 (merge h1 b2)

-- part c
merge2'
  :: Ord t =>
     t
     -> WeightLeftistHeap t
     -> WeightLeftistHeap t
     -> WeightLeftistHeap t
     -> WeightLeftistHeap t
merge2' root l r1 r2
  | wl > wr = Tree w root l r
  | otherwise = Tree w root r l
  where r = merge2 r1 r2
        wl = weight l
        wr = weight r
        w = 1 + wl + wr

merge2
  :: Ord t =>
     WeightLeftistHeap t -> WeightLeftistHeap t -> WeightLeftistHeap t
merge2 h Empty = h
merge2 Empty h = h
merge2 h1@(Tree _ x a1 b1) h2@(Tree _ y a2 b2)
  | x < y = merge2' x h2 a1 b1
  | otherwise = merge2' y h1 a2 b2

-- part d
-- You immediatly have the root of the tree, without having to build the entire tree
-- yet. Constructs down as necessary

insert :: Ord a => a -> WeightLeftistHeap a -> WeightLeftistHeap a
insert e h = merge (singleton e) h

findMin :: WeightLeftistHeap a -> Maybe a
findMin Empty = Nothing
findMin (Tree _ x _ _) = Just x

deleteMin :: Ord a => WeightLeftistHeap a -> Maybe (WeightLeftistHeap a)
deleteMin Empty = Nothing
deleteMin (Tree _ _ a b) = Just $ merge a b

singleton :: k -> WeightLeftistHeap k
singleton e = Tree 1 e Empty Empty
