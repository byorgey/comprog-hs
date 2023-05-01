module Trie where

import           Control.Monad              ((>=>))
import qualified Data.ByteString.Lazy.Char8 as C
import           Data.List                  (foldl')
import           Data.Map                   (Map, (!))
import qualified Data.Map                   as M
import           Data.Maybe                 (fromMaybe, isJust)

data Trie a = Trie
  { trieSize :: !Int
  , value    :: !(Maybe a)
  , children :: !(Map Char (Trie a))
  }
  deriving Show

emptyTrie :: Trie a
emptyTrie = Trie 0 Nothing M.empty

-- | Insert a new key/value pair into a trie, updating the size
--   appropriately.
insert :: C.ByteString -> a -> Trie a -> Trie a
insert w a t = fst (go w t)
  where
    go = C.foldr
      (\c ins (Trie n v m) ->
         let (t', ds) = ins (fromMaybe emptyTrie (M.lookup c m))
         in  (Trie (n+ds) v (M.insert c t' m), ds)
      )
      (\(Trie n v m) ->
         let ds = if isJust v then 0 else 1
         in  (Trie (n+ds) (Just a) m, ds)
      )

-- | Create an initial trie from a list of key/value pairs.  If there
--   are multiple pairs with the same key, later pairs override
--   earlier ones.
mkTrie :: [(C.ByteString, a)] -> Trie a
mkTrie = foldl' (flip (uncurry insert)) emptyTrie

-- | Look up a single character in a trie, returning the corresponding
--   child trie (if any).
lookup1 :: Char -> Trie a -> Maybe (Trie a)
lookup1 c = M.lookup c . children

-- | Look up a string key in a trie, returning the corresponding value
--   (if any).
lookup :: C.ByteString -> Trie a -> Maybe a
lookup = C.foldr ((>=>) . lookup1) value

-- | Fold a trie into a summary value.
foldTrie :: (Int -> Maybe a -> Map Char r -> r) -> Trie a -> r
foldTrie f (Trie n b m) = f n b (M.map (foldTrie f) m)

-- | "Drive" a trie by repeatedly looking up consecutive characters
--   from a string, restarting at the root every time we "fall off"
--   the bottom, and returning the list of all values encountered
--   along the way.  Note that this function will crash if it ever
--   looks up a character which is not in the current trie.
--
--   This is of particular use in decoding using a prefix-free code.
drive :: Trie a -> C.ByteString -> [a]
drive t = reverse . snd . C.foldl' step (t, [])
  where
    step (s, as) c =
      let Just s' = lookup1 c s
      in  maybe (s', as) (\a -> (t, a:as)) (value s')
