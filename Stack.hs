-- https://byorgey.github.io/blog/posts/2024/11/27/stacks-queues.html
{-# LANGUAGE BangPatterns #-}

module Stack where

import Data.List (foldl')

data Stack m a = Stack (a -> m) !Int [(m, a)]

instance (Show m, Show a) => Show (Stack m a) where
  show (Stack _ _ as) = show as

instance (Eq m, Eq a) => Eq (Stack m a) where
  Stack _ _ as1 == Stack _ _ as2 = as1 == as2

new :: (a -> m) -> Stack m a
new f = Stack f 0 []

size :: Stack m a -> Int
size (Stack _ n _) = n

measure :: Monoid m => Stack m a -> m
measure (Stack _ _ as) = case as of
  [] -> mempty
  (m, _) : _ -> m

push :: Monoid m => a -> Stack m a -> Stack m a
push a s@(Stack f n as) = Stack f (n + 1) ((f a <> measure s, a) : as)

pop :: Stack m a -> Maybe (a, Stack m a)
pop (Stack f n as) = case as of
  [] -> Nothing
  (_, a) : as' -> Just (a, Stack f (n - 1) as')

reverse :: Monoid m => Stack m a -> Stack m a
reverse (Stack f _ as) = foldl' (flip push) (new f) (map snd as)
