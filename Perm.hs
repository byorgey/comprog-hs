{-# LANGUAGE BangPatterns #-}

module Perm where

import           Control.Arrow
import           Control.Monad.ST
import           Data.Array.Base
import           Data.Array.MArray
import           Data.Array.ST
import           Data.Array.Unboxed
import           Data.Array.Unsafe  (unsafeFreeze)

-- | 'Perm' represents a /1-indexed/ permutation.  It can also be
--   thought of as an endofunction on the set @{1 .. n}@.
type Perm = UArray Int Int

-- | Construct a 'Perm' from a list containing a permutation of the
--   numbers 1..n.  The resulting 'Perm' sends @i@ to whatever number
--   is at index @i-1@ in the list.
fromList :: [Int] -> Perm
fromList xs = listArray (1,length xs) xs

-- | Compose two permutations (corresponds to backwards function
--   composition).  Only defined if the permutations have the same
--   size.
andThen :: Perm -> Perm -> Perm
andThen p1 p2 = listArray (bounds p1) (map ((p1!) >>> (p2!)) (range (bounds p1)))

-- | Compute the inverse of a permutation.
inverse :: Perm -> Perm
inverse p = array (bounds p) [ (p!k, k) | k <- range (bounds p) ]

data CycleDecomp = CD
  { cycleID     :: UArray Int Int  -- | Each number maps to the ID #
                                   --   of the cycle it is part of
  , cycleLen    :: UArray Int Int  -- | Each cycle ID maps to the length of that cycle
  , cycleIndex  :: UArray Int Int  -- | Each element maps to its (0-based) index in its cycle
  , cycleCounts :: UArray Int Int  -- | Each size maps to the number of cycles of that size
  }
  deriving Show



-- | Cycle decomposition of a permutation, using mutable arrays.
permToCycles :: Perm -> CycleDecomp
permToCycles p = cd where

  (_,n) = bounds p

  cd = runST $ do
    cid <- newArray (1,n) 0
    cix <- newArray (1,n) 0
    ccs <- newArray (1,n) 0

    lens <- findCycles cid cix ccs 1 1
    cid' <- freeze cid
    cix' <- freeze cix
    ccs' <- freeze ccs
    return $ CD cid' (listArray (1,length lens) lens) cix' ccs'

  findCycles :: STUArray s Int Int -> STUArray s Int Int -> STUArray s Int Int
    -> Int -> Int -> ST s [Int]
  findCycles cid cix ccs l !k
    | k > n     = return []
    | otherwise = do
        id <- readArray cid k
        case id of
          0 -> do
            len <- labelCycle cid cix l k 0

            count <- readArray ccs len
            writeArray ccs len (count+1)

            (len:) <$> findCycles cid cix ccs (l+1) (k+1)
          _ -> findCycles cid cix ccs l (k+1)

  labelCycle cid cix l k !i = do
    id <- readArray cid k
    case id of
      0 -> do
        writeArray cid k l
        writeArray cix k i
        (1+) <$> labelCycle cid cix l (p!k) (i+1)
      _ -> return 0

