module Trie where

import           Control.Monad              ((>=>))
import qualified Data.ByteString.Lazy.Char8 as C
import           Data.List                  (foldl')
import           Data.Map                   (Map, (!))
import qualified Data.Map                   as M
import           Data.Maybe                 (fromMaybe)

data Trie a = Trie
  { trieSize :: !Int
  , value    :: !(Maybe a)
  , children :: !(Map Char (Trie a))
  }
  deriving Show

emptyTrie :: Trie a
emptyTrie = Trie 0 Nothing M.empty

insert :: C.ByteString -> a -> Trie a -> Trie a
insert w a = C.foldr
  (\c ins (Trie n b m) -> Trie (n+1) b $ M.insert c (ins $ fromMaybe emptyTrie (M.lookup c m)) m)
  (\(Trie n _ m) -> Trie (n+1) (Just a) m)
  w

mkTrie :: [(C.ByteString, a)] -> Trie a
mkTrie = foldl' (flip (uncurry insert)) emptyTrie

lookup1 :: Char -> Trie a -> Maybe (Trie a)
lookup1 c = M.lookup c . children

lookup :: C.ByteString -> Trie a -> Maybe a
lookup = C.foldr ((>=>) . lookup1) value

foldTrie :: (Int -> Maybe a -> Map Char r -> r) -> Trie a -> r
foldTrie f (Trie n b m) = f n b (M.map (foldTrie f) m)
