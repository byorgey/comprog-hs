-- Adapted from https://kseo.github.io/posts/2014-01-30-implementing-union-find-in-haskell.html
-- https://byorgey.github.io/blog/posts/2024/11/02/UnionFind.html
-- https://byorgey.github.io/blog/posts/2024/11/18/UnionFind-sols.html
{-# LANGUAGE RecordWildCards #-}

module UnionFind where

import Control.Monad (when)
import Control.Monad.ST
import Data.Array.ST

type Node = Int

data UnionFind s m = UnionFind
  { parent :: !(STUArray s Node Node)
  , sz :: !(STUArray s Node Int)
  , ann :: !(STArray s Node m)
  }

new :: Int -> m -> ST s (UnionFind s m)
new n m = newWith n (const m)

newWith :: Int -> (Node -> m) -> ST s (UnionFind s m)
newWith n m =
  UnionFind
    <$> newListArray (0, n - 1) [0 .. n - 1]
    <*> newArray (0, n - 1) 1
    <*> newListArray (0, n - 1) (map m [0 .. n - 1])

connected :: UnionFind s m -> Node -> Node -> ST s Bool
connected uf x y = (==) <$> find uf x <*> find uf y

find :: UnionFind s m -> Node -> ST s Node
find uf@(UnionFind {..}) x = do
  p <- readArray parent x
  if p /= x
    then do
      r <- find uf p
      writeArray parent x r
      pure r
    else pure x

updateAnn :: Semigroup m => UnionFind s m -> Node -> (m -> m) -> ST s ()
updateAnn uf@(UnionFind {..}) x f = do
  x <- find uf x
  old <- readArray ann x -- modifyArray is not available in Kattis test environment
  writeArray ann x (f old)

union :: Semigroup m => UnionFind s m -> Node -> Node -> ST s ()
union uf@(UnionFind {..}) x y = do
  x <- find uf x
  y <- find uf y
  when (x /= y) $ do
    sx <- readArray sz x
    sy <- readArray sz y
    mx <- readArray ann x
    my <- readArray ann y
    if sx < sy
      then do
        writeArray parent x y
        writeArray sz y (sx + sy)
        writeArray ann y (mx <> my)
      else do
        writeArray parent y x
        writeArray sz x (sx + sy)
        writeArray ann x (mx <> my)

size :: UnionFind s m -> Node -> ST s Int
size uf@(UnionFind {..}) x = do
  x <- find uf x
  readArray sz x

getAnn :: UnionFind s m -> Node -> ST s m
getAnn uf@(UnionFind {..}) x = do
  x <- find uf x
  readArray ann x

allAnns :: UnionFind s m -> ST s [(Int, m)]
allAnns UnionFind {..} = do
  ps <- getAssocs parent
  flip foldMap ps $ \(p, x) ->
    if p == x
      then do
        a <- readArray ann x
        s <- readArray sz x
        pure [(s, a)]
      else pure []
