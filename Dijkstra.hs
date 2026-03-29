{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Dijkstra where

import Debug.Trace

import Enumeration

import Control.Arrow (second, (>>>))
import Control.Monad
import Control.Monad.ST
import Data.Array.IArray qualified as IA
import Data.Array.ST
import Data.Array.Unboxed (UArray)
import Data.Array.Unboxed qualified as U
import Data.Array.Unsafe (unsafeFreeze)
import Data.Sequence (Seq (..), ViewL (..), (<|), (|>))
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as S

------------------------------------------------------------
-- Utilities
------------------------------------------------------------

infixl 0 >$>
(>$>) :: a -> (a -> b) -> b
(>$>) = flip ($)
{-# INLINE (>$>) #-}

exhaustM :: Monad m => (a -> m (Maybe a)) -> a -> m a
exhaustM f = go
 where
  go a = do
    ma <- f a
    maybe (return a) go ma

------------------------------------------------------------
-- Dijkstra
------------------------------------------------------------

data DijkstraResult v c = DijkstraR {getCost :: v -> Maybe c, getParent :: v -> Maybe v}

type V = Int
data DijkstraState s c = DS
  { cost :: STUArray s V c
  , parent :: STUArray s V V
  , pq :: Set (c, V)
  }

initDijkstraState ::
  (Ord c, Num c, forall s. MArray (STUArray s) c (ST s)) =>
  Int ->
  [Int] ->
  ST s (DijkstraState s c)
initDijkstraState n vs = do
  c <- newArray (0, n - 1) (-1)
  p <- newArray (0, n - 1) (-1)

  forM_ vs $ \v -> writeArray c v 0
  return $ DS c p (S.fromList $ map (0,) vs)

dijkstra ::
  forall v c.
  (Show c, Ord c, Num c, forall s. MArray (STUArray s) c (ST s), U.IArray UArray c) =>
  Enumeration v ->
  [v] ->
  (v -> [(c, v)]) ->
  (v -> Bool) ->
  DijkstraResult v c
dijkstra Enumeration {..} vs next goal =
  toResult $ dijkstra' card (map locate vs) (map (second locate) . next . select) (goal . select)
 where
  toResult :: (forall s. ST s (DijkstraState s c)) -> DijkstraResult v c
  toResult m = runST $ do
    st <- m
    (cost' :: UArray V c) <- unsafeFreeze (cost st)
    (parent' :: UArray V V) <- unsafeFreeze (parent st)
    return $
      DijkstraR
        ((\c -> guard (c /= -1) >> Just c) . (cost' IA.!) . locate)
        ((\p -> guard (p /= -1) >> Just (select p)) . (parent' IA.!) . locate)

visited :: DijkstraState s c -> V -> ST s Bool
visited DS {..} v = (/= -1) <$> readArray parent v
{-# INLINE visited #-}

dijkstra' ::
  (Show c, Ord c, Num c, forall s. MArray (STUArray s) c (ST s)) =>
  Int ->
  [V] ->
  (V -> [(c, V)]) ->
  (V -> Bool) ->
  ST s (DijkstraState s c)
dijkstra' n vs next goal = do
  st <- initDijkstraState n vs
  exhaustM dijkstraStep st
 where
  dijkstraStep st@DS {..} = traceShow pq $ case S.minView pq of
    Nothing -> return Nothing
    Just ((_, v), pq')
      | goal v -> return Nothing
      | otherwise ->
          v >$> next
            >>> foldM (upd v) (st {pq = pq'})
            >>> fmap Just

  upd ::
    forall s c.
    (Ord c, Num c, MArray (STUArray s) c (ST s)) =>
    V ->
    DijkstraState s c ->
    (c, V) ->
    ST s (DijkstraState s c)
  upd p d@DS {..} (c, v) = do
    parentCost <- readArray cost p
    oldCost <- readArray cost v
    let newCost = parentCost + c
    if (oldCost == -1 || newCost < oldCost)
      then do
        writeArray cost v newCost
        writeArray parent v p
        return $ d {pq = S.insert (newCost, v) (S.delete (oldCost, v) pq)}
      else return d
