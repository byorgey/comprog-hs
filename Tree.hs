-- https://byorgey.github.io/blog/posts/2024/07/11/cpih-factor-full-tree.html
-- https://byorgey.github.io/blog/posts/2024/08/08/TreeDecomposition.html

module Tree where

import Control.Arrow ((***))
import Control.Category ((>>>))
import Data.Bifunctor (second)
import Data.List (maximumBy)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Map (Map, (!?))
import qualified Data.Map as M
import Data.Ord (comparing)
import Data.Tree
import Data.Tuple (swap)

edgesToMap :: Ord a => [(a, a)] -> Map a [a]
edgesToMap = concatMap (\p -> [p, swap p]) >>> dirEdgesToMap

dirEdgesToMap :: Ord a => [(a, a)] -> Map a [a]
dirEdgesToMap = map (second (: [])) >>> M.fromListWith (++)

mapToTree :: Ord a => (a -> [b] -> b) -> Map a [a] -> a -> b
mapToTree nd m root = go root root
 where
  go parent root = nd root (maybe [] (map (go root) . filter (/= parent)) (m !? root))

edgesToTree :: Ord a => (a -> [b] -> b) -> [(a, a)] -> a -> b
edgesToTree nd = mapToTree nd . edgesToMap

parentMap :: Ord a => Tree a -> Map a a
parentMap = foldTree node >>> snd
 where
  node a b = (a, M.fromList (map (,a) as) <> mconcat ms)
   where
    (as, ms) = unzip b

type SubtreeSelector a = a -> [Tree a] -> Maybe (Tree a, [Tree a])

pathDecomposition :: (a -> [Tree a] -> Maybe (Tree a, [Tree a])) -> Tree a -> [NonEmpty a]
pathDecomposition select = go
 where
  go = selectPath select >>> second (concatMap go) >>> uncurry (:)

selectPath :: SubtreeSelector a -> Tree a -> (NonEmpty a, [Tree a])
selectPath select = go
 where
  go (Node a ts) = case select a ts of
    Nothing -> (NE.singleton a, ts)
    Just (t, ts') -> ((a NE.<|) *** (ts' ++)) (go t)

type Height = Int
type Size = Int

labelHeight :: Tree a -> Tree (Height, a)
labelHeight = foldTree node
 where
  node a ts = case ts of
    [] -> Node (0, a) []
    _ -> Node (1 + maximum (map (fst . rootLabel) ts), a) ts

labelSize :: Tree a -> Tree (Size, a)
labelSize = foldTree $ \a ts -> Node (1 + sum (map (fst . rootLabel) ts), a) ts

-- | Decompose a tree into chains by length, first the longest
--   possible chain, then the longest chain from what remains, and so
--   on.
maxChainDecomposition :: Tree a -> [NonEmpty (Height, a)]
maxChainDecomposition =
  labelHeight
    >>> pathDecomposition (const (selectMaxBy (comparing (fst . rootLabel))))
    >>> sortBy (comparing (Down . fst . NE.head))

selectMaxBy :: (a -> a -> Ordering) -> [a] -> Maybe (a, [a])
selectMaxBy _ [] = Nothing
selectMaxBy cmp (a : as) = case selectMaxBy cmp as of
  Nothing -> Just (a, [])
  Just (b, bs) -> case cmp a b of
    LT -> Just (b, a : bs)
    _ -> Just (a, b : bs)

-- | Heavy-light decomposition of a tree.
heavyLightDecomposition :: Tree a -> [NonEmpty (Size, a)]
heavyLightDecomposition =
  labelSize >>> pathDecomposition (const (selectMaxBy (comparing (fst . rootLabel))))
