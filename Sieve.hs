-- https://codeforces.com/blog/entry/54090
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Sieve where

import Control.Monad (forM_, unless)
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed
import Data.STRef

newSTUArray :: (MArray (STUArray s) e (ST s), Ix i) => (i, i) -> e -> ST s (STUArray s i e)
newSTUArray = newArray

-- | Basic version.  Suppose we want to compute a multiplicative function f such that
--   - f(p) = g(p) for prime p
--   - f(ip) = h(i, p, f(i)) when p divides i
-- Then @sieve n g h@ returns an array f on (1,n) such that f!i = f(i), in O(n) time.
sieve :: Int -> (Int -> Int) -> (Int -> Int -> Int -> Int) -> UArray Int Int
sieve n g h = runSTUArray $ do
  primes <- newArray (0, n) (0 :: Int)
  numPrimes <- newSTRef 0
  composite <- newSTUArray (0, n) False
  f <- newArray (1, n) 1
  forM_ [2 .. n] $ \i -> do
    isComp <- readArray composite i
    unless isComp $ do
      appendPrime primes numPrimes i
      writeArray f i (g i)

    np <- readSTRef numPrimes

    let markComposites j
          | j >= np = return ()
          | otherwise = do
              p <- readArray primes j
              let ip = i * p
              case ip > n of
                True -> return ()
                False -> do
                  writeArray composite ip True
                  fi <- readArray f i
                  case i `mod` p of
                    0 -> writeArray f ip (h i p fi)
                    _ -> do
                      writeArray f ip (fi * g p)
                      markComposites (j + 1)

    markComposites 0
  return f

appendPrime :: STUArray s Int Int -> STRef s Int -> Int -> ST s ()
appendPrime primes numPrimes p = do
  n <- readSTRef numPrimes
  writeArray primes n p
  writeSTRef numPrimes $! n + 1

-- | More general version.  Suppose we want to compute a
--   multiplicative function f such that f(p^k) = g p k.  Then
--   @genSieve n g@ returns an array f on (1,n) such that f!i = f(i),
--   in O(n) time (but with a slightly higher constant factor than
--   'sieve').
genSieve :: Int -> (Int -> Int -> Int) -> UArray Int Int
genSieve n g = runSTUArray $ do
  primes <- newArray (0, n) (0 :: Int)
  numPrimes <- newSTRef 0
  composite <- newSTUArray (0, n) False
  count <- newSTUArray (2, n) (0 :: Int)
  f <- newArray (1, n) 1
  forM_ [2 .. n] $ \i -> do
    isComp <- readArray composite i
    unless isComp $ do
      appendPrime primes numPrimes i
      writeArray f i (g i 1)
      writeArray count i 1

    np <- readSTRef numPrimes

    let markComposites j
          | j >= np = return ()
          | otherwise = do
              p <- readArray primes j
              let ip = i * p
              case ip > n of
                True -> return ()
                False -> do
                  writeArray composite ip True
                  fi <- readArray f i
                  case i `mod` p of
                    0 -> do
                      c <- readArray count i
                      f' <- readArray f (i `div` (p ^ c))
                      writeArray f ip (f' * g p (c + 1))
                      writeArray count ip (c + 1)
                    _ -> do
                      writeArray f ip (fi * g p 1)
                      writeArray count ip 1
                      markComposites (j + 1)

    markComposites 0
  return f

-- Some examples!

-- Euler's phi (totient) function
phi :: Int -> UArray Int Int
phi n = sieve n (subtract 1) (\_ p fi -> fi * p)

-- Divisor sigma function, σₐ(x) = Σ{d | x} dᵃ
divisorSigma :: Int -> Int -> UArray Int Int
divisorSigma n a = genSieve n (\p k -> (p ^ (a * (k + 1)) - 1) `div` (p ^ a - 1))

-- Möbius function (μ)
mu :: Int -> UArray Int Int
mu n = sieve n (const (-1)) (\_ _ _ -> 0)
