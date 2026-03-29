module SegTree where

import Data.List (find)
import Data.Maybe (fromMaybe)

data SegTree m
  = Leaf !Int !m
  | -- Node i j corresponds to range [i,j)
    Node !Int !Int !m !(SegTree m) !(SegTree m)
  deriving (Show)

node :: Semigroup m => SegTree m -> SegTree m -> SegTree m
node l r = Node (getLeft l) (getRight r) (getValue l <> getValue r) l r

getValue :: SegTree m -> m
getValue (Leaf _ m) = m
getValue (Node _ _ m _ _) = m

getLeft :: SegTree m -> Int
getLeft (Leaf i _) = i
getLeft (Node i _ _ _ _) = i

getRight :: SegTree m -> Int
getRight (Leaf i _) = i + 1
getRight (Node _ j _ _ _) = j

mk :: Semigroup m => [m] -> SegTree m
mk as = go 0 (length as) as
 where
  go i _ [a] = Leaf i a
  go i j as = node l r
   where
    (as1, as2) = splitAt h as
    h = (j - i) `div` 2
    l = go i (i + h) as1
    r = go (i + h) j as2

update :: Semigroup m => Int -> (m -> m) -> SegTree m -> SegTree m
update _ f (Leaf i m) = Leaf i (f m)
update p f (Node _ _ _ l r)
  | p < getLeft r = node (update p f l) r
  | otherwise = node l (update p f r)

set :: Semigroup m => Int -> m -> SegTree m -> SegTree m
set p = update p . const

get :: Int -> SegTree m -> m
get p (Leaf _ m) = m
get p (Node _ _ _ l r)
  | p < getLeft r = get p l
  | otherwise = get p r

-- x, y *inclusive*
range :: Semigroup m => Int -> Int -> SegTree m -> m
range x y (Leaf _ m) = m
range x y (Node i j m l r)
  | x <= i && y >= j - 1 = m
  | y < getLeft r = range x y l
  | x >= getLeft r = range x y r
  | otherwise = range x y l <> range x y r
