-- https://byorgey.wordpress.com/2020/02/07/competitive-programming-in-haskell-primes-and-factoring/
-- https://byorgey.wordpress.com/2020/02/15/competitive-programming-in-haskell-modular-arithmetic-part-1/
-- https://byorgey.wordpress.com/2020/03/03/competitive-programming-in-haskell-modular-arithmetic-part-2/

module NumberTheory where

import qualified Data.Foldable as F
import Data.Map (Map)
import qualified Data.Map as M

import Control.Arrow
import Data.Bits
import Data.List (group, sort)
import Data.Maybe (fromJust)

------------------------------------------------------------
-- Modular exponentiation

modexp :: Integer -> Integer -> Integer -> Integer
modexp _ 0 _ = 1
modexp b e m
  | e .&. 1 == 0 = (r * r) `mod` m
  | otherwise = (b * r * r) `mod` m
 where
  r = modexp b (e `shiftR` 1) m

------------------------------------------------------------
-- (Extended) Euclidean algorithm

-- egcd a b = (g,x,y)
--   g is the gcd of a and b, and ax + by = g
egcd :: Integer -> Integer -> (Integer, Integer, Integer)
egcd a 0
  | a < 0 = (-a, -1, 0)
  | otherwise = (a, 1, 0)
egcd a b = (g, y, x - (a `div` b) * y)
 where
  (g, x, y) = egcd b (a `mod` b)

-- g = bx + (a mod b)y
--   = bx + (a - b(a/b))y
--   = ay + b(x - (a/b)y)

-- inverse p a  is the multiplicative inverse of a mod p
inverse :: Integer -> Integer -> Integer
inverse p a = y `mod` p
 where
  (_, _, y) = egcd p a

------------------------------------------------------------
-- Primes, factoring, and divisors

--------------------------------------------------
-- Miller-Rabin primality testing

smallPrimes = take 9 primes

-- https://en.wikipedia.org/wiki/Miller%E2%80%93Rabin_primality_test#Testing_against_small_sets_of_bases
-- smallPrimes n
--   | n < 2047 = [2]
--   | n < 1373653 = [2,3]
--   | n < 9080191 = [31, 73]
--   | n < 25326001 = [2, 3, 5]
--   | n < 3215031751 = [2, 3, 5, 7]
--   | n < 4759123141 = [2, 7, 61]
--   | n < 1122004669633 = [2, 13, 23, 1662803]
--   | n < 2152302898747 = [2, 3, 5, 7, 11]
--   | n < 3474749660383 = [2, 3, 5, 7, 11, 13]
--   | n < 341550071728321 = [2, 3, 5, 7, 11, 13, 17]
--   | n < 3825123056546413051 = [2, 3, 5, 7, 11, 13, 17, 19, 23]
--   | n < 18446744073709551616 = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37]

-- With these values of a, guaranteed to work up to 3*10^18 (see https://pastebin.com/6XEFRPaZ)
isPrime :: Integer -> Bool
isPrime n
  | n `elem` smallPrimes = True
  | any ((== 0) . (n `mod`)) smallPrimes = False
  | otherwise = all (spp n) smallPrimes

-- spp n a tests whether n is a strong probable prime to base a.
spp :: Integer -> Integer -> Bool
spp n a = (ad == 1) || (n - 1) `elem` as
 where
  (s, d) = decompose (n - 1)
  ad = modexp a d n
  as = take s (iterate ((`mod` n) . (^ 2)) ad)

-- decompose n = (s,d) such that n = 2^s * d and d is odd.
decompose :: Integer -> (Int, Integer)
decompose n
  | odd n = (0, n)
  | otherwise = first succ (decompose (n `shiftR` 1))

--------------------------------------------------
-- Pollard Rho factoring algorithm

-- Tries to find a non-trivial factor of the given number, using the
-- given starting value.
pollardRho :: Integer -> Integer -> Maybe Integer
pollardRho a n = go (g a) (g (g a))
 where
  go x y
    | d == n = Nothing
    | d == 1 = go (g x) (g (g y))
    | otherwise = Just d
   where
    d = gcd (abs (x - y)) n
  g x = (x * x + 1) `mod` n

-- Find a nontrivial factor of a number we know for sure is composite.
compositeFactor :: Integer -> Integer
compositeFactor n | even n = 2
compositeFactor 25 = 5
compositeFactor n = fromJust (F.asum (map (`pollardRho` n) [2 ..]))

--------------------------------------------------
-- Factoring

factorMap :: Integer -> Map Integer Int
factorMap = factor >>> M.fromList

factor :: Integer -> [(Integer, Int)]
factor = listFactors >>> group >>> map (head &&& length)

primes :: [Integer]
primes = 2 : sieve primes [3 ..]
 where
  sieve (p : ps) xs =
    let (h, t) = span (< p * p) xs
     in h ++ sieve ps (filter ((/= 0) . (`mod` p)) t)

listFactors :: Integer -> [Integer]
listFactors = sort . go
 where
  go 1 = []
  go n
    | isPrime n = [n]
    | otherwise = go d ++ go (n `div` d)
   where
    d = compositeFactor n

-- listFactors :: Integer -> [Integer]
-- listFactors = go primes
--   where
--     go _ 1      = []
--     go (p:ps) n
--       | p*p > n = [n]
--       | n `mod` p == 0 = p : go (p:ps) (n `div` p)
--       | otherwise      = go ps n

divisors :: Integer -> [Integer]
divisors =
  factor
    >>> map (\(p, k) -> take (k + 1) (iterate (* p) 1))
    >>> sequence
    >>> map product

totient :: Integer -> Integer
totient = factor >>> map (\(p, k) -> p ^ (k - 1) * (p - 1)) >>> product

------------------------------------------------------------
-- Solving modular equations

-- solveMod a b m  solves  ax = b (mod m), returning (y,k) such that all
-- solutions are equivalent to  y (mod k)
solveMod :: Integer -> Integer -> Integer -> Maybe (Integer, Integer)
solveMod a b m
  | g == 1 = Just ((b * inverse m a) `mod` m, m)
  | b `mod` g == 0 = solveMod (a `div` g) (b `div` g) (m `div` g)
  | otherwise = Nothing
 where
  g = gcd a m

-- gcrt solves a system of modular equations.  Each equation x = a
-- (mod n) is given as a pair (a,n).  Returns a pair (z, k) such that
-- 0 <= z < k and solutions for x satisfy x = z (mod k), that is,
-- solutions are of the form x = z + kt for integer t.
gcrt :: [(Integer, Integer)] -> Maybe (Integer, Integer)
gcrt [e] = Just e
gcrt (e1 : e2 : es) = gcrt2 e1 e2 >>= \e -> gcrt (e : es)

-- gcrt2 (a,n) (b,m) solves the pair of modular equations
--
--   x = a (mod n)
--   x = b (mod m)
--
-- It returns a pair (c, k) such that 0 <= c < k and all solutions for
-- x satisfy x = c (mod k), that is, solutions are of the form x = c +
-- kt for integer t.
gcrt2 :: (Integer, Integer) -> (Integer, Integer) -> Maybe (Integer, Integer)
gcrt2 (a, n) (b, m)
  | a `mod` g == b `mod` g = Just (((a * v * m + b * u * n) `div` g) `mod` k, k)
  | otherwise = Nothing
 where
  (g, u, v) = egcd n m
  k = (m * n) `div` g
