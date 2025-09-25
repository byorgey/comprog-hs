-- https://byorgey.wordpress.com/2021/11/15/competitive-programming-in-haskell-enumeration/

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Enumeration where

import qualified Data.List as L

import qualified Data.Array as A
import qualified Data.Map.Strict as M

data Enumeration a = Enumeration
  { card   :: !Int
  , select :: Int -> a
  , locate :: a -> Int
  }

-- | Map a pair of inverse functions over an invertible enumeration of
--   @a@ values to turn it into an invertible enumeration of @b@
--   values.  Because invertible enumerations contain a /bijection/ to
--   the natural numbers, we really do need both directions of a
--   bijection between @a@ and @b@ in order to map.  This is why
--   'Enumeration' cannot be an instance of 'Functor'.
mapE :: (a -> b) -> (b -> a) -> Enumeration a -> Enumeration b
mapE f g (Enumeration c s l) = Enumeration c (f . s) (l . g)

-- | List the elements of an enumeration in order.  Inverse of
--   'listE'.
enumerate :: Enumeration a -> [a]
enumerate e = map (select e) [0 .. card e-1]

-- | The empty enumeration, with cardinality zero and no elements.
voidE :: Enumeration a
voidE = Enumeration 0 (error "select void") (error "locate void")

-- | The unit enumeration, with a single value of @()@ at index 0.
unitE :: Enumeration ()
unitE = singletonE ()

-- | An enumeration of a single given element at index 0.
singletonE :: a -> Enumeration a
singletonE a = Enumeration 1 (const a) (const 0)

-- | A finite prefix of the natural numbers.
finiteE :: Int -> Enumeration Int
finiteE n = Enumeration n id id

-- | Construct an enumeration from the elements of a finite list.
--   The elements of the list must all be distinct. To turn an
--   enumeration back into a list, use 'enumerate'.
listE :: forall a. Ord a => [a] -> Enumeration a
listE as = Enumeration n (toA A.!) (fromA M.!)
  where
    n = length as
    toA :: A.Array Int a
    toA = A.listArray (0,n-1) as

    fromA :: M.Map a Int
    fromA = M.fromList (zip as [0 :: Int ..])

-- | Enumerate all the values of a bounded 'Enum' instance.
boundedEnum :: forall a. (Enum a, Bounded a) => Enumeration a
boundedEnum = Enumeration
  { card   = hi - lo + 1
  , select = toEnum . (+lo)
  , locate = subtract lo . fromEnum
  }
  where
    lo, hi :: Int
    lo = fromIntegral (fromEnum (minBound @a))
    hi = fromIntegral (fromEnum (maxBound @a))

-- | Sum, /i.e./ disjoint union, of two enumerations. All the values
--   of the first are enumerated before the values of the second.
(>+<) :: Enumeration a -> Enumeration b -> Enumeration (Either a b)
a >+< b = Enumeration
  { card   = card a + card b
  , select = \k -> if k < card a then Left (select a k) else Right (select b (k - card a))
  , locate = either (locate a) ((+card a) . locate b)
  }

-- | Cartesian product of enumerations, with a lexicographic ordering.
(>*<) :: Enumeration a -> Enumeration b -> Enumeration (a,b)
a >*< b = Enumeration
  { card = card a * card b
  , select = \k -> let (i,j) = k `divMod` card b in (select a i, select b j)
  , locate = \(x,y) -> card b * locate a x + locate b y
  }

-- | Take a finite prefix from the beginning of an enumeration.  @takeE
--   k e@ always yields the empty enumeration for \(k \leq 0\), and
--   results in @e@ whenever @k@ is greater than or equal to the
--   cardinality of the enumeration.  Otherwise @takeE k e@ has
--   cardinality @k@ and matches @e@ from @0@ to @k-1@.
takeE :: Int -> Enumeration a -> Enumeration a
takeE k e
  | k <= 0      = voidE
  | k >= card e = e
  | otherwise   = Enumeration k (select e) (locate e)

-- | Drop some elements from the beginning of an enumeration.  @dropE k
--   e@ yields @e@ unchanged if \(k \leq 0\), and results in the empty
--   enumeration whenever @k@ is greater than or equal to the
--   cardinality of @e@.
dropE :: Int -> Enumeration a -> Enumeration a
dropE k e
  | k <= 0      = e
  | k >= card e = voidE
  | otherwise   = Enumeration
      { card = card e - k
      , select = select e . (+k)
      , locate = subtract k . locate e
      }

-- | Zip two enumerations in parallel, producing the pair of
--   elements at each index.  The resulting enumeration is truncated
--   to the cardinality of the smaller of the two arguments.
zipE :: Enumeration a -> Enumeration b -> Enumeration (a,b)
zipE ea eb = Enumeration
  { card = min (card ea) (card eb)
  , select = \k -> (select ea k, select eb k)
  , locate = locate ea . fst
  }
