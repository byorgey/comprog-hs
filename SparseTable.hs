{-# LANGUAGE TupleSections #-}

module SparseTable where

import Data.Array (Array, array, (!))
import Data.Bifunctor (first)
import Data.Bits
import IdempotentSemigroup

newtype SparseTable m = SparseTable (Array (Int, Int) m)
  deriving (Show)

-- | Logarithm base 2, rounded down to the nearest integer.  Computed
--   efficiently using primitive bitwise instructions.
lg :: Int -> Int
lg n = finiteBitSize n - 1 - countLeadingZeros n

-- | Construct a sparse table which can answer range queries over the
--   given list in $O(1)$ time.  Constructing the sparse table takes
--   $O(n \lg n)$ time and space, where $n$ is the length of the list.
fromList :: IdempotentSemigroup m => [m] -> SparseTable m
fromList ms = SparseTable st
 where
  n = length ms
  lgn = lg n

  st =
    array ((0, 0), (lgn, n - 1)) $
      (map (first (0,)) $ zip [0 ..] ms)
        ++ [ ((i, j), st ! (i - 1, j) <> st ! (i - 1, j + 1 !<<. (i - 1)))
           | i <- [1 .. lgn]
           , j <- [0 .. n - 1 !<<. i]
           ]

-- | \$O(1)$. @range st l r@ computes the range query which is the
--   @sconcat@ of all the elements from index @l@ to @r@ (inclusive).
range :: IdempotentSemigroup m => SparseTable m -> Int -> Int -> m
range (SparseTable st) l r = st ! (i, l) <> st ! (i, r - (1 !<<. i) + 1)
 where
  i = lg (r - l + 1)
