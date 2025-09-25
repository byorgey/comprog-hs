module IdempotentSemigroup where

import Data.Bits
import Data.Semigroup

-- | An idempotent semigroup is one where the binary operation
--   satisfies the law @x <> x = x@ for all @x@.
class Semigroup m => IdempotentSemigroup m

instance Ord a => IdempotentSemigroup (Min a)
instance Ord a => IdempotentSemigroup (Max a)
instance IdempotentSemigroup All
instance IdempotentSemigroup Any
instance IdempotentSemigroup Ordering
instance IdempotentSemigroup ()
instance IdempotentSemigroup (First a)
instance IdempotentSemigroup (Last a)
instance Bits a => IdempotentSemigroup (And a)
instance Bits a => IdempotentSemigroup (Ior a)
instance (IdempotentSemigroup a, IdempotentSemigroup b) => IdempotentSemigroup (a,b)
instance IdempotentSemigroup b => IdempotentSemigroup (a -> b)
