{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns          #-}

module SegTree where

import           Data.List  (find)
import           Data.Maybe (fromMaybe)

class Action m s where
  act :: m -> s -> s

data SegTree m a
  = Leaf !Int !a
  | Node !Int !Int !a !m (SegTree m a) (SegTree m a)
  deriving Show

instance Action () s where
  act _ = id

node :: (Action m a, Semigroup a) => m -> SegTree m a -> SegTree m a -> SegTree m a
node m l r = Node (getLeft l) (getRight r) (act m (getValue l <> getValue r)) m l r

getValue :: SegTree m a -> a
getValue (Leaf _ a)         = a
getValue (Node _ _ a _ _ _) = a

getLeft :: SegTree m a -> Int
getLeft (Leaf i _)         = i
getLeft (Node i _ _ _ _ _) = i

getRight :: SegTree m a -> Int
getRight (Leaf i _)         = i+1
getRight (Node _ j _ _ _ _) = j

mkSegTree :: (Monoid m, Monoid a, Action m a) => [a] -> SegTree m a
mkSegTree as = go 1 n (as ++ replicate (n - length as) mempty)
  where
    Just n = find (>= length as) (iterate (*2) 1)

    go i _ [a] = Leaf i a
    go i j as = node mempty l r
      where
        (as1, as2) = splitAt h as
        h = (j-i+1) `div` 2
        l = go i (i+h-1) as1
        r = go (i+h) j   as2

push :: (Monoid m, Action m a) => SegTree m a -> SegTree m a
push (Node i j a m l r) = Node i j a mempty (applyAct m l) (applyAct m r)
push t@Leaf{}           = t

applyAct :: (Monoid m, Action m a) => m -> SegTree m a -> SegTree m a
applyAct m (Leaf i a)          = Leaf i (act m a)
applyAct m (Node i j a m2 l r) = Node i j (act m a) (m <> m2) l r

update :: (Monoid m, Semigroup a, Action m a) => Int -> (a -> a) -> SegTree m a -> SegTree m a
update _ f (Leaf i a) = Leaf i (f a)
update p f (push -> Node _ _ _ _ l r)
  | p < getLeft r = node mempty (update p f l) r
  | otherwise     = node mempty l (update p f r)

set :: (Monoid m, Semigroup a, Action m a) => Int -> a -> SegTree m a -> SegTree m a
set p = update p . const

get :: (Monoid m, Action m a) => Int -> SegTree m a -> a
get p (Leaf _ a) = a
get p (push -> Node _ _ _ _ l r)
  | p < getLeft r = get p l
  | otherwise     = get p r

range :: (Monoid m, Monoid a, Action m a) => Int -> Int -> SegTree m a -> a
range x y _ | x == y = mempty
range x y (Leaf i a)
  | x <= i && i < y = a
  | otherwise = mempty
range x y (push -> Node i j _ _ l r)
  | y <= i || j <= x = mempty
  | otherwise = range x y l <> range x y r

apply :: (Monoid m, Semigroup a, Action m a) => Int -> m -> SegTree m a -> SegTree m a
apply p = update p . act

applyRange :: (Monoid m, Semigroup a, Action m a) => Int -> Int -> m -> SegTree m a -> SegTree m a
applyRange x y _ t | x == y = t
applyRange x y m l@(Leaf i a)
  | x <= i && i < y = Leaf i (act m a)
  | otherwise = l
applyRange x y m n@(Node i j a m' l r)
  | x <= i && j <= y = Node i j a (m <> m') l r
  | otherwise = case push n of
      Node _ _ _ _ l r -> node mempty (applyRange x y m l) (applyRange x y m r)

startingFrom :: (Monoid m, Action m a) => Int -> SegTree m a -> [SegTree m a]
startingFrom l t = go t []
  where
    go t@(Leaf i _)
      | l <= i = (t:)
      | otherwise = id
    go (push -> t@(Node i j _ _ lt rt))
      | l <= i = (t:)
      | l < getLeft rt = go lt . (rt:)
      | l < getRight rt = go rt
      | otherwise = id

-- | Preconditions:
--
--   - @l <= getRight t@
--   - @g(mempty) == True@
--   - @g@ is antitone, that is, if @g(a)@ is false then so is @g(a <> b)@.
--
--   Given these preconditions, @maxRight l g t@ returns the biggest
--   @r@ such that @g (range l r t) == True@ but @g (range l (r+1) t)
--   == False@ (or @r = getRight t@ if there is no such @r@).
maxRight :: (Monoid a, Monoid m, Action m a) => Int -> (a -> Bool) -> SegTree m a -> Int
maxRight l g t = fromMaybe (getRight t) (go mempty (startingFrom l t))
  where
    go _ [] = Nothing
    go cur (Leaf i a : ts)
      | g (cur <> a) = go (cur <> a) ts
      | otherwise    = Just i
    go cur ((push -> Node i j a _ lt rt) : ts)
      | g (cur <> a) = go (cur <> a) ts
      | otherwise    = go cur (lt : rt : ts)

minLeft :: (Monoid a, Monoid m, Action m a) => Int -> (a -> Bool) -> SegTree m a -> Int
minLeft = undefined
