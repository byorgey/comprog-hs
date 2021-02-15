{-# LANGUAGE TupleSections #-}

module Util where

import           Control.Arrow
import           Data.Array.IArray
import           Data.Function
import           Data.List
import           Data.Maybe
import           Data.Ord

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

fj :: Maybe a -> a
fj = fromJust

both :: (a -> b) -> (a,a) -> (b,b)
both f (x,y) = (f x, f y)

sortGroupOn :: Ord b => (a -> b) -> [a] -> [(b,[a])]
sortGroupOn f = sortOn f >>> groupBy ((==) `on` f) >>> map ((f.head) &&& id)

pairs :: [a] -> [(a,a)]
pairs []     = []
pairs (a:as) = map (a,) as ++ pairs as

withPairs :: Monoid r => (a -> a -> r) -> [a] -> r
withPairs _ []     = mempty
withPairs _ [_]    = mempty
withPairs f (a:as) = go as
  where
    go []        = withPairs f as
    go (a2:rest) = f a a2 <> go rest

-- Discrete binary search.  Find the smallest integer in [lo,hi] such
-- that monotone predicate p holds.
binarySearchD :: Int -> Int -> (Int -> Bool) -> Int
binarySearchD lo hi p
  | lo == hi = lo
  | p mid     = binarySearchD lo mid p
  | otherwise = binarySearchD (mid+1) hi p
  where
    mid = (lo + hi) `div` 2

generate :: (Ix i, IArray a e) => (i,i) -> (i -> e) -> a i e
generate rng f = listArray rng (map f (range rng))

arraydef :: (Ix i, IArray a e) => (i,i) -> e -> [(i,e)] -> a i e
arraydef rng def vs = array rng ([(i,def) | i <- range rng] ++ vs)
