-- https://cp-algorithms.com/data_structures/sqrt-tree.html
-- https://cp-algorithms.com/data_structures/sqrt_decomposition.html

{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTSyntax #-}

module SqrtTree where

import Data.Array (Array, array, listArray, (!), bounds)
import Data.List (scanl1, scanr1)

data Block m = Block
  { total :: m             -- ^ total of this entire block
  , prefix :: Array Int m  -- ^ prefix sums for this block
  , suffix :: Array Int m  -- ^ suffix sums for this block
  , subtree :: SqrtTree m  -- ^ sqrt tree for this block
  }
  deriving (Show)

data SqrtTree m where
  One :: m -> SqrtTree m
  Two :: m -> m -> SqrtTree m
  Branch ::
    Int ->                     -- ^ block size
    Array Int (Block m) ->     -- ^ blocks
    Array (Int,Int) m ->       -- ^ between sums for blocks
    SqrtTree m
  deriving (Show)

fromList :: Semigroup m => [m] -> SqrtTree m
fromList ms = fromArray $ listArray (0,length ms-1) ms

fromArray :: Semigroup m => Array Int m -> SqrtTree m
fromArray ms = mkSqrtTree ms lo (hi+1)
 where
  (lo,hi) = bounds ms

-- | @mkSqrtTree ms lo hi@ makes a sqrt tree on ms[lo..hi).
mkSqrtTree :: Semigroup m => Array Int m -> Int -> Int -> SqrtTree m
mkSqrtTree ms lo hi
  | hi - lo == 1 = One (ms ! lo)
  | hi - lo == 2 = Two (ms ! lo) (ms ! (lo+1))
  | otherwise = Branch k blocks between
  where
    k :: Int
    k = ceiling (sqrt @Double (fromIntegral (hi - lo)))

    -- blocks :: Array Int (Block m)
    blocks = listArray (0,k-1) . map mkBlock . takeWhile (< hi) . iterate (+k) $ lo

    mkBlock i = Block (pref!(n-1)) pref suf (mkSqrtTree ms i j)
      where
        n = j-i
        elts = map (ms!) [i..j-1]
        pref = listArray (0,n-1) (scanl1 (<>) elts)
        suf = listArray (0,n-1) (scanr1 (<>) elts)
        j = min (i + k) hi

    -- between :: Array (Int,Int) m
    between = array ((0,0), (k-1, k-1)) $
      [((s,s), total (blocks!s)) | s <- [0 .. k-1]]
      ++ [ ((s,t), between!(s,t-1) <> total (blocks!t))
         | s <- [0 .. k-2]
         , t <- [s+1 .. k-1]
         ]
      -- XXX only to print out
      ++ [ ((t,s), between!(s,t-1) <> total (blocks!t))
         | s <- [0 .. k-2]
         , t <- [s+1 .. k-1]
         ]

range :: Semigroup m => SqrtTree m -> Int -> Int -> m
range (One m) _ _ = m
range (Two x y) 0 0 = x
range (Two x y) 0 1 = x <> y
range (Two x y) 1 1 = y
range (Branch k blocks between) l r
  | lb == rb = range (subtree (blocks!lb)) li ri
  | rb - lb == 1 = suffix (blocks!lb) ! li <> prefix (blocks!rb) ! ri
  | otherwise = suffix (blocks!lb)!li <> between!(lb+1,rb-1) <> prefix (blocks!rb) ! ri
  where
   (lb, li) = l `divMod` k
   (rb, ri) = r `divMod` k
