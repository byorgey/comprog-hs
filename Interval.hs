module Interval (I, lo, hi, mkI, uncons, splits, subs, length, take, drop, range) where

import           Prelude hiding (drop, length, take)

data I = I { lo :: !Int, hi :: !Int }
  deriving (Eq, Ord, Show)

mkI :: Int -> Int -> I
mkI l h
  | l == h = I 0 0
  | otherwise = I l h

uncons :: I -> I
uncons (I l h) = mkI (l+1) h

splits :: I -> [(I, I)]
splits (I l h) = [(mkI l m, mkI m h) | m <- [l .. h]]

subs :: I -> [I]
subs (I l h) = mkI 0 0 : [mkI l' h' | l' <- [l .. h-1], h' <- [l'+1 .. h]]

length :: I -> Int
length (I l h) = h - l

take :: Int -> I -> I
take k (I l h) = mkI l (min (l+k) h)

drop :: Int -> I -> I
drop k (I l h) = mkI (min (l+k) h) h

range :: I -> [Int]
range (I l h) = [l .. h-1]

