-- https://byorgey.wordpress.com/2020/02/07/competitive-programming-in-haskell-primes-and-factoring/
-- https://byorgey.wordpress.com/2020/02/15/competitive-programming-in-haskell-modular-arithmetic-part-1/
-- https://byorgey.wordpress.com/2020/03/03/competitive-programming-in-haskell-modular-arithmetic-part-2/

module NumberTheory where

import           Data.Map      (Map)
import qualified Data.Map      as M

import           Control.Arrow
import           Data.List     (group, sort)

------------------------------------------------------------
-- Modular exponentiation

modexp :: Integer -> Integer -> Integer -> Integer
modexp _ 0 _ = 1
modexp b e m
  | even e    = (r*r) `mod` m
  | otherwise = (b*r*r) `mod` m
  where
    r = modexp b (e `div` 2) m

------------------------------------------------------------
-- (Extended) Euclidean algorithm

-- egcd a b = (g,x,y)
--   g is the gcd of a and b, and ax + by = g
egcd :: Integer -> Integer -> (Integer, Integer, Integer)
egcd a 0
  | a < 0     = (-a,-1,0)
  | otherwise = (a,1,0)
egcd a b = (g, y, x - (a `div` b) * y)
  where
    (g,x,y) = egcd b (a `mod` b)

    -- g = bx + (a mod b)y
    --   = bx + (a - b(a/b))y
    --   = ay + b(x - (a/b)y)

-- inverse p a  is the multiplicative inverse of a mod p
inverse :: Integer -> Integer -> Integer
inverse p a = y `mod` p
  where
    (_,_,y) = egcd p a

------------------------------------------------------------
-- Primes, factoring, and divisors

factorMap :: Integer -> Map Integer Int
factorMap = factor >>> M.fromList

factor :: Integer -> [(Integer, Int)]
factor = listFactors >>> group >>> map (head &&& length)

primes :: [Integer]
primes = 2 : sieve primes [3..]
  where
    sieve (p:ps) xs =
      let (h,t) = span (< p*p) xs
      in  h ++ sieve ps (filter ((/=0).(`mod`p)) t)

listFactors :: Integer -> [Integer]
listFactors = go primes
  where
    go _ 1      = []
    go (p:ps) n
      | p*p > n = [n]
      | n `mod` p == 0 = p : go (p:ps) (n `div` p)
      | otherwise      = go ps n

divisors :: Integer -> [Integer]
divisors = factor >>> map (\(p,k) -> take (k+1) (iterate (*p) 1)) >>>
  sequence >>> map product

totient :: Integer -> Integer
totient = factor >>> map (\(p,k) -> p^(k-1) * (p-1)) >>> product

------------------------------------------------------------
-- Solving modular equations

-- solveMod a b m  solves  ax = b (mod m), returning (y,k) such that all
-- solutions are equivalent to  y (mod k)
solveMod :: Integer -> Integer -> Integer -> Maybe (Integer, Integer)
solveMod a b m
  | g == 1    = Just ((b * inverse m a) `mod` m, m)
  | b `mod` g == 0 = solveMod (a `div` g) (b `div` g) (m `div` g)
  | otherwise = Nothing
  where
    g = gcd a m

-- gcrt solves a system of modular equations.  Each equation x = a
-- (mod n) is given as a pair (a,n).  Returns a pair (z, k) such that
-- solutions for x satisfy x = z (mod k), that is, solutions are of
-- the form x = kt + c for integer t.
gcrt :: [(Integer, Integer)] -> Maybe (Integer, Integer)
gcrt [e]        = Just e
gcrt (e1:e2:es) = gcrt2 e1 e2 >>= \e -> gcrt (e:es)

-- gcrt2 (a,n) (b,m) solves the pair of modular equations
--
--   x = a (mod n)
--   x = b (mod m)
--
-- It returns a pair (c, k) such that all solutions for x satisfy x =
-- c (mod k), that is, solutions are of the form x = kt + c for
-- integer t.
gcrt2 :: (Integer, Integer) -> (Integer, Integer) -> Maybe (Integer, Integer)
gcrt2 (a,n) (b,m)
  | a `mod` g == b `mod` g = Just (((a*v*m + b*u*n) `div` g) `mod` k, k)
  | otherwise              = Nothing
  where
    (g,u,v) = egcd n m
    k = (m*n) `div` g
