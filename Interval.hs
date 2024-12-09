module Interval where

import Prelude hiding (drop, length, take)

data I = I {lo :: !Int, hi :: !Int}
    deriving (Eq, Ord, Show)

mkI :: Int -> Int -> I
mkI l h
    | l == h = I 0 0
    | otherwise = I l h

(∪), (∩) :: I -> I -> I
I l1 h1 ∪ I l2 h2 = I (min l1 l2) (max h1 h2)
I l1 h1 ∩ I l2 h2 = I (max l1 l2) (min h1 h2)

isEmpty :: I -> Bool
isEmpty (I l h) = l > h

(⊆) :: I -> I -> Bool
i1 ⊆ i2 = i1 ∪ i2 == i2

point :: Int -> I
point x = I x x

uncons :: I -> I
uncons (I l h) = mkI (l + 1) h

splits :: I -> [(I, I)]
splits (I l h) = [(mkI l m, mkI m h) | m <- [l .. h]]

subs :: I -> [I]
subs (I l h) = mkI 0 0 : [mkI l' h' | l' <- [l .. h - 1], h' <- [l' + 1 .. h]]

length :: I -> Int
length (I l h) = h - l

take :: Int -> I -> I
take k (I l h) = mkI l (min (l + k) h)

drop :: Int -> I -> I
drop k (I l h) = mkI (min (l + k) h) h

range :: I -> [Int]
range (I l h) = [l .. h - 1]
