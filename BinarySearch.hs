-- https://byorgey.wordpress.com/2023/01/01/competitive-programming-in-haskell-better-binary-search/
-- https://julesjacobs.com/notes/binarysearch/binarysearch.pdf

module BinarySearch where

import           Data.Bits
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
--       - Use 'fwd' or 'bwd' to do linear search through a range.
--       - If you are feeling adventurous, use 'floating' to do
--         precise binary search over the bit representation of
--         'Double' values.
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
  | r - l > 1 = Just (l + (r-l) `div` 2)
  | otherwise = Nothing

-- | Step function for continuous binary search.  Stops once
--   @r - l <= eps@; otherwise returns their midpoint.
continuous :: (Fractional a, Ord a) => a -> a -> a -> Maybe a
continuous eps l r
  | r - l > eps = Just (l + (r-l)/2)
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

------------------------------------------------------------
-- Binary search on floating-point representations

-- A lot of blood, sweat, and tears went into these functions.  Are
-- they even correct?  Was it worth it?  Who knows! See:
--
-- https://byorgey.wordpress.com/2023/01/01/competitive-programming-in-haskell-better-binary-search/#comment-40882
-- https://web.archive.org/web/20220326204603/http://stereopsis.com/radix.html

-- | Step function for precise binary search over the bit
--   representation of @Double@ values.
floating :: Double -> Double -> Maybe Double
floating l r = b2f <$> binary (f2b l) (f2b r)

-- | A monotonic conversion from 'Double' to 'Word64'.  That is,
--   @x < y@ iff @f2b x < f2b y@.  'b2f' is its inverse.
f2b :: Double -> Word64
f2b 0 = 0x7fffffffffffffff
f2b x
  = (if m < 0 then 0 else bit 63)
  `xor` flipNeg (eBits `xor` mBits)
  where
    (m, e) = decodeFloat x
    eBits = fromIntegral (e + d + bias) `shiftL` d
    mBits = fromIntegral (abs m) `clearBit` d

    d = floatDigits x - 1
    bias = 1023
    flipNeg
      | m < 0 = (`clearBit` 63) . complement
      | otherwise = id

prop_f2b_monotonic :: Double -> Double -> Bool
prop_f2b_monotonic x y = (x < y) == (f2b x < f2b y)

-- | The left inverse of 'f2b', that is, for all @x :: Double@, @b2f
--   (f2b x) == x@.  Note @f2b (b2f x) == x@ does not strictly hold
--   since not every @Word64@ value corresponds to a valid @Double@
--   value.
b2f :: Word64 -> Double
b2f 0x7fffffffffffffff = 0
b2f w = encodeFloat m (fromIntegral e - bias - d)
  where
    s = testBit w 63
    w' = (if s then id else complement) w

    d = floatDigits (1 :: Double) - 1
    bias = 1023
    m = (if s then id else negate) (fromIntegral ((w' .&. ((1 `shiftL` d) - 1)) `setBit` d))
    e = clearBit w' 63 `shiftR` d

prop_b2f_f2b :: Double -> Bool
prop_b2f_f2b x = b2f (f2b x) == x

-- Some Word64 values correspond to +/-Infinity or NaN.  For most
-- others, f2b is inverse to b2f; for a few that represent very tiny
-- floating-point values, the Word64 resulting from a round trip may
-- differ by 1.
prop_f2b_b2f :: Word64 -> Bool
prop_f2b_b2f w = isInfinite x || isNaN x || dist (f2b x) w <= 1
  where
    x = b2f w
    dist x y
      | x < y = y - x
      | otherwise = x - y

-- Given two distinct Double values, if we take the midpoint of their
-- corresponding Word64 values, we get another Word64 that represents
-- a floating point number strictly in between the original two.
prop_f2b_mid_monotonic :: Double -> Double -> Bool
prop_f2b_mid_monotonic x y = x == y || (x' < z && z < y')
  where
    x' = min x y
    y' = max x y
    l = f2b x'
    r = f2b y'
    m = l + (r-l) `div` 2
    z = b2f m
