{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}

module Slice where

import           Control.Arrow      ((***))
import           Data.Array.Unboxed as U
import           Interval           (I)
import qualified Interval           as I
import           Prelude            as P

data Slice a = S { storage :: !(UArray Int a), interval :: !I }

deriving instance (Show a, IArray UArray a) => Show (Slice a)

{-# SPECIALIZE fromList :: String -> Slice Char #-}
fromList :: IArray UArray a => [a] -> Slice a
fromList s = S (listArray (0,n-1) s) (I.mkI 0 n)
  where
    n = P.length s

{-# SPECIALIZE toList :: Slice Char -> String #-}
toList :: IArray UArray a => Slice a -> [a]
toList (S s i) = map (s U.!) $ I.range i

length :: Slice a -> Int
length (S _ i) = I.length i

take :: Int -> Slice a -> Slice a
take k (S s i) = S s (I.take k i)

subs :: Slice a -> [Slice a]
subs (S s i) = map (S s) (I.subs i)

splits :: Slice a -> [(Slice a, Slice a)]
splits (S s i) = map (S s *** S s) (I.splits i)

{-# SPECIALIZE (Slice.!) :: Slice Char -> Int -> Char #-}
(!) :: IArray UArray a => Slice a -> Int -> a
(S s i) ! k = s U.! (k + I.lo i)

range :: Slice a -> [Int]
range (S _ i) = map (subtract (I.lo i)) (I.range i)
