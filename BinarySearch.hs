-- XXX blog post
-- https://julesjacobs.com/notes/binarysearch/binarysearch.pdf

module BinarySearch where

import           Data.Word     (Word64)
import           Unsafe.Coerce (unsafeCoerce)

-- | Generic search function.  Takes a step function, a predicate, and
--   low and high values, and it finds values (l,r) right next to each
--   other where the predicate switches from False to True.
--
--   More specifically, for @search mid p l r@:
--     - PRECONDITIONS:
--       - @(p l)@ must be @False@ and @(p r)@ must be @True@!
--       - If using one of the binary search step functions, the
--         predicate @p@ must be monotonic, that is, intuitively, @p@
--         must switch from @False@ to @True@ exactly once.  Formally, if
--         @x <= y@ then @p x <= p y@ (where @False <= True@).
--     - The mid function is called as @mid l r@ to find the next
--       index to search on the interval [l, r].  @mid l r@ must
--       return a value strictly between @l@ and @r@.
--       - Use 'binary' to do binary search over the integers.
--       - Use @('continuous' eps)@ to do binary search over rational
--         or floating point values.  (l,r) are returned such that r -
--         l <= eps.
--       - Use 'binaryFloat' to do precise binary search over the
--         bit representation of 'Double' values.
--       - Use 'fwd' or 'bwd' to do linear search through a range.
--     - Returns (l,r) such that:
--       - @not (p l) && p r@
--       - @mid l r == Nothing@
search :: (a -> a -> Maybe a) -> (a -> Bool) -> a -> a -> (a,a)
search mid p = go
  where
    go l r = case mid l r of
      Nothing -> (l,r)
      Just m
        | p m       -> go l m
        | otherwise -> go m r

-- | Step function for binary search over an integral type.  Stops
--   when @r - l <= 1@; otherwise returns their midpoint.
binary :: Integral a => a -> a -> Maybe a
binary l r
  | r - l > 1 = Just ((l+r) `div` 2)
  | otherwise = Nothing

-- | Step function for continuous binary search.  Stops once
--   @r - l <= eps@; otherwise returns their midpoint.
continuous :: (Fractional a, Ord a) => a -> a -> a -> Maybe a
continuous eps l r
  | r - l > eps = Just ((l+r) / 2)
  | otherwise = Nothing

-- | Step function for forward linear search.  Stops when @r - l <=
--   1$; otherwise returns @l + 1@.
fwd :: (Num a, Ord a) => a -> a -> Maybe a
fwd l r
  | r - l > 1 = Just (l+1)
  | otherwise = Nothing

-- | Step function for backward linear search.  Stops when @r - l <=
--   1$; otherwise returns @r - 1@.
bwd :: (Num a, Ord a) => a -> a -> Maybe a
bwd l r
  | r - l > 1 = Just (r-1)
  | otherwise = Nothing

-- | Step function for precise binary search over the bit
--   representation of @Double@ values.
binaryFloat :: Double -> Double -> Maybe Double
binaryFloat l r = decode <$> binary (encode l) (encode r)
  where
    encode :: Double -> Word64
    encode = unsafeCoerce

    decode :: Word64 -> Double
    decode = unsafeCoerce
