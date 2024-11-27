-- https://byorgey.github.io/blog/posts/2024/11/27/stacks-queues.html
{-# LANGUAGE ImportQualifiedPost #-}

module Queue where

import Data.Bifunctor (second)
import Data.List (foldl')
import Stack (Stack)
import Stack qualified as Stack

type CommutativeMonoid = Monoid

data Queue m a = Queue {getFront :: Stack m a, getBack :: Stack m a}
  deriving (Show, Eq)

new :: (a -> m) -> Queue m a
new f = Queue (Stack.new f) (Stack.new f)

size :: Queue m a -> Int
size (Queue front back) = Stack.size front + Stack.size back

measure :: CommutativeMonoid m => Queue m a -> m
measure (Queue front back) = Stack.measure front <> Stack.measure back

enqueue :: CommutativeMonoid m => a -> Queue m a -> Queue m a
enqueue a (Queue front back) = Queue front (Stack.push a back)

dequeue :: CommutativeMonoid m => Queue m a -> Maybe (a, Queue m a)
dequeue (Queue front back)
  | Stack.size front == 0 && Stack.size back == 0 = Nothing
  | Stack.size front == 0 = dequeue (Queue (Stack.reverse back) front)
  | otherwise = second (\front' -> Queue front' back) <$> Stack.pop front

drop1 :: CommutativeMonoid m => Queue m a -> Queue m a
drop1 q = case dequeue q of
  Nothing -> q
  Just (_, q') -> q'

------------------------------------------------------------
-- Sliding windows

-- @windows w f as@ computes the monoidal sum @foldMap f window@
-- for each w-@window@ (i.e. contiguous subsequence of length @w@) of
-- @as@, in only O(length as) time.  For example, @windows 3 Sum
-- [4,1,2,8,3] = [7, 11, 13]@, and @windows 3 Max [4,1,2,8,3] = [4,8,8]@.
windows :: CommutativeMonoid m => Int -> (a -> m) -> [a] -> [m]
windows w f as = go startQ rest
 where
  (start, rest) = splitAt w as
  startQ = foldl' (flip enqueue) (new f) start

  go q as =
    measure q : case as of
      [] -> []
      a : as -> go (enqueue a (drop1 q)) as

data Max a = NegInf | Max a deriving (Eq, Ord, Show)

instance Ord a => Semigroup (Max a) where
  NegInf <> a = a
  a <> NegInf = a
  Max a <> Max b = Max (max a b)

instance Ord a => Monoid (Max a) where
  mempty = NegInf

data Min a = Min a | PosInf deriving (Eq, Ord, Show)

instance Ord a => Semigroup (Min a) where
  PosInf <> a = a
  a <> PosInf = a
  Min a <> Min b = Min (min a b)

instance Ord a => Monoid (Min a) where
  mempty = PosInf
