-- https://byorgey.wordpress.com/2020/07/18/competitive-programming-in-haskell-cycle-decomposition-with-mutable-arrays/
{-# LANGUAGE BangPatterns #-}

module Perm where

import Control.Arrow
import Control.Monad.ST
import Data.Array.Base
import Data.Array.MArray
import Data.Array.ST
import Data.Array.Unboxed

-- | 'Perm' represents a /1-indexed/ permutation.  It can also be
--   thought of as an endofunction on the set @{1 .. n}@.
newtype Perm = Perm {getPerm :: UArray Int Int}
  deriving (Eq, Ord, Show)

idPerm :: Int -> Perm
idPerm n = fromList [1 .. n]

-- | Construct a 'Perm' from a list containing a permutation of the
--   numbers 1..n.  The resulting 'Perm' sends @i@ to whatever number
--   is at index @i-1@ in the list.
fromList :: [Int] -> Perm
fromList xs = Perm $ listArray (1, length xs) xs

-- | Compose two permutations (corresponds to backwards function
--   composition).  Only defined if the permutations have the same
--   size.
andThen :: Perm -> Perm -> Perm
andThen (Perm p1) (Perm p2) = Perm $ listArray (bounds p1) (map ((p1 !) >>> (p2 !)) (range (bounds p1)))

instance Semigroup Perm where
  (<>) = andThen

-- | Compute the inverse of a permutation.
inverse :: Perm -> Perm
inverse (Perm p) = Perm $ array (bounds p) [(p ! k, k) | k <- range (bounds p)]

data CycleDecomp = CD
  { cycleID :: UArray Int Int
  , cycleLen :: UArray Int Int
  -- ^ Each number maps to the ID #
  --   of the cycle it is part of
  , cycleIndex :: UArray Int Int
  -- ^ Each cycle ID maps to the length of that cycle
  , cycleCounts :: UArray Int Int
  -- ^ Each element maps to its (0-based) index in its cycle
  }
  -- \| Each size maps to the number of cycles of that size

  deriving (Show)

-- | Cycle decomposition of a permutation in O(n), using mutable arrays.
permToCycles :: Perm -> CycleDecomp
permToCycles (Perm p) = cd
 where
  (_, n) = bounds p

  cd = runST $ do
    cid <- newArray (1, n) 0
    cix <- newArray (1, n) 0
    ccs <- newArray (1, n) 0

    lens <- findCycles cid cix ccs 1 1
    cid' <- freeze cid
    cix' <- freeze cix
    ccs' <- freeze ccs
    return $ CD cid' (listArray (1, length lens) lens) cix' ccs'

  findCycles ::
    STUArray s Int Int ->
    STUArray s Int Int ->
    STUArray s Int Int ->
    Int ->
    Int ->
    ST s [Int]
  findCycles cid cix ccs l !k -- l = next available cycle ID; k = cur element
    | k > n = return []
    | otherwise = do
        -- check if k is already marked as part of a cycle
        id <- readArray cid k
        case id of
          0 -> do
            -- k is unvisited.  Explore its cycle and label it as l.
            len <- labelCycle cid cix l k 0

            -- Remember that we have one more cycle of this size.
            count <- readArray ccs len
            writeArray ccs len (count + 1)

            -- Continue with the next label and the next element, and
            -- remember the size of this cycle
            (len :) <$> findCycles cid cix ccs (l + 1) (k + 1)

          -- k is already visited: just go on to the next element
          _ -> findCycles cid cix ccs l (k + 1)

  -- Explore a single cycle, label all its elements and return its size.
  labelCycle cid cix l k !i = do
    -- Keep going as long as the next element is unlabelled.
    id <- readArray cid k
    case id of
      0 -> do
        -- Label the current element with l.
        writeArray cid k l
        -- The index of the current element is i.
        writeArray cix k i

        -- Look up the next element in the permutation and continue.
        (1 +) <$> labelCycle cid cix l (p ! k) (i + 1)
      _ -> return 0
