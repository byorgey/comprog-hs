-- https://byorgey.wordpress.com/2021/10/14/competitive-programming-in-haskell-bfs-part-1/
-- https://byorgey.wordpress.com/2021/10/18/competitive-programming-in-haskell-bfs-part-2-alternative-apis/
-- https://byorgey.wordpress.com/2021/10/29/competitive-programming-in-haskell-bfs-part-3-implementation-via-hashmap/
-- https://byorgey.wordpress.com/2021/11/15/competitive-programming-in-haskell-enumeration/

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Graph where

import Enumeration

import           Control.Arrow       ((>>>))
import           Control.Monad
import           Control.Monad.ST
import           Data.Array.IArray   (IArray, listArray)
import qualified Data.Array.IArray   as IA
import           Data.Array.ST
import           Data.Array.Unboxed  (UArray)
import qualified Data.Array.Unboxed  as U
import           Data.Array.Unsafe   (unsafeFreeze)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.Hashable
import           Data.Sequence       (Seq (..), ViewL (..), (<|), (|>))
import qualified Data.Sequence       as Seq
import           Data.Set            (Set)
import qualified Data.Set            as S

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
-- BFS
------------------------------------------------------------

data BFSResult v =
  BFSR { getLevel :: v -> Maybe Int, getParent :: v -> Maybe v }

type V = Int
data BFSState s =
  BS { level :: STUArray s V Int, parent :: STUArray s V V, queue :: Seq V }

initBFSState :: Int -> [Int] -> ST s (BFSState s)
initBFSState n vs = do
  l <- newArray (0,n-1) (-1)
  p <- newArray (0,n-1) (-1)

  forM_ vs $ \v -> writeArray l v 0
  return $ BS l p (Seq.fromList vs)

bfs :: forall v. Enumeration v -> [v] -> (v -> [v]) -> (v -> Bool) -> BFSResult v
bfs Enumeration{..} vs next goal
  = toResult $ bfs' card (map locate vs) (map locate . next . select) (goal . select)
  where
    toResult :: (forall s. ST s (BFSState s)) -> BFSResult v
    toResult m = runST $ do
      st <- m
      (level' :: UArray V Int) <- unsafeFreeze (level st)
      (parent' :: UArray V V) <- unsafeFreeze (parent st)
      return $
        BFSR
          ((\l -> guard (l /= -1) >> Just l) . (level' IA.!) . locate)
          ((\p -> guard (p /= -1) >> Just (select p)) . (parent' IA.!) . locate)

visited :: BFSState s -> V -> ST s Bool
visited BS{..} v = (/= -1) <$> readArray level v
{-# INLINE visited #-}

bfs' :: Int -> [V] -> (V -> [V]) -> (V -> Bool) -> ST s (BFSState s)
bfs' n vs next goal = do
  st <- initBFSState n vs
  exhaustM bfsStep st
  where
    bfsStep st@BS{..} = case Seq.viewl queue of
      EmptyL -> return Nothing
      v :< q'
        | goal v -> return Nothing
        | otherwise -> v >$> next >>> filterM (fmap not . visited st)
            >=> foldM (upd v) (st{queue=q'}) >>> fmap Just

    upd p b@BS{..} v = do
      lp <- readArray level p
      writeArray level v (lp + 1)
      writeArray parent v p
      return $ b{queue = queue |> v}
