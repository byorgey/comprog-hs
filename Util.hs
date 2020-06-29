{-# LANGUAGE TupleSections #-}

module Util where

import           Control.Arrow
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
sortGroupOn f = sortBy (comparing f) >>> groupBy ((==) `on` f) >>> map ((f.head) &&& id)

pairs :: [a] -> [(a,a)]
pairs []     = []
pairs [_]    = []
pairs (a:as) = map (a,) as ++ pairs as

withPairs :: Monoid r => (a -> a -> r) -> [a] -> r
withPairs _ []     = mempty
withPairs _ [_]    = mempty
withPairs f (a:as) = go as
  where
    go []        = withPairs f as
    go (a2:rest) = f a a2 <> go rest
