{-# LANGUAGE MultiParamTypeClasses #-}

module Heap where

class Ord k => Heap h k where
  empty :: h
  isEmpty :: h -> Bool
  insert :: k -> h -> h
  merge :: h -> h -> h
  findMin :: h -> k
  deleteMin :: h -> h
