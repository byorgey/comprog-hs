-- https://byorgey.wordpress.com/2020/06/24/competitive-programming-in-haskell-vectors-and-2d-geometry/

{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Geom where

------------------------------------------------------------
-- 2D points and vectors

data V2 s = V2 { getX :: !s, getY :: !s } deriving (Eq, Ord, Show, Functor)
type V2D  = V2 Double

type P2 s = V2 s
type P2D  = P2 Double

instance Foldable V2 where
  foldMap f (V2 x y) = f x <> f y

zero :: Num s => V2 s
zero = V2 0 0

-- | Adding and subtracting vectors.
(^+^), (^-^) :: Num s => V2 s -> V2 s -> V2 s
V2 x1 y1 ^+^ V2 x2 y2 = V2 (x1+x2) (y1+y2)
V2 x1 y1 ^-^ V2 x2 y2 = V2 (x1-x2) (y1-y2)

-- | Scalar multiple of a vector.
(*^) :: Num s => s -> V2 s -> V2 s
(*^) k = fmap (k*)

------------------------------------------------------------
-- Utilities

-- | This combinator allows us to write e.g. 'v2 int' or 'v2 double'
--   to get a 'Scanner (V2 s)'.
v2 :: Applicative f => f s -> f (V2 s)
v2 s = V2 <$> s <*> s

------------------------------------------------------------
-- Angles

newtype Angle = A Double  -- angle (radians)
  deriving (Show, Eq, Ord, Num, Fractional, Floating)

fromDeg :: Double -> Angle
fromDeg d = A (d * pi / 180)

fromRad :: Double -> Angle
fromRad = A

toDeg :: Angle -> Double
toDeg (A r) = r * 180 / pi

toRad :: Angle -> Double
toRad (A r) = r

-- | Construct a vector in polar coordinates.
fromPolar :: Double -> Angle -> V2D
fromPolar r θ = rot θ (V2 r 0)

-- | Rotate a vector counterclockwise by a given angle.
rot :: Angle -> V2D -> V2D
rot (A θ) (V2 x y) = V2 (cos θ * x - sin θ * y) (sin θ * x + cos θ * y)

------------------------------------------------------------
-- Cross product

-- | 2D cross product of two vectors.  Gives the signed area of their
--   parallelogram (positive iff the second is counterclockwise of the
--   first).  [Geometric algebra tells us that this is really the
--   coefficient of the bivector resulting from the outer product of
--   the two vectors.]
--
--   Note this works even for integral scalar types.
cross :: Num s => V2 s -> V2 s -> s
cross (V2 ux uy) (V2 vx vy) = ux*vy - vx*uy

-- | A version of cross product specialized to three points describing
--   the endpoints of the vectors.  The first argument is the shared
--   tail of the vectors, and the second and third arguments are the
--   endpoints of the vectors.
crossP :: Num s => P2 s -> P2 s -> P2 s -> s
crossP p1 p2 p3 = cross (p2 ^-^ p1) (p3 ^-^ p1)

-- | The signed area of a triangle with given vertices can be computed
--   as half the cross product of two of the edges.
--
--   Note that this requires 'Fractional' because of the division by
--   two.  If you want to stick with integral scalars, you can just
--   use 'crossP' to get twice the signed area.
signedTriArea :: Fractional s => P2 s -> P2 s -> P2 s -> s
signedTriArea p1 p2 p3 = crossP p1 p2 p3 / 2

-- | The (nonnegative) area of the triangle with the given vertices.
triArea :: Fractional s => P2 s -> P2 s -> P2 s -> s
triArea p1 p2 p3 = abs (signedTriArea p1 p2 p3)

-- | The signed area of the polygon with the given vertices, via the
--   "shoelace formula". Positive iff the points are given in
--   counterclockwise order.
signedPolyArea :: Fractional s => [P2 s] -> s
signedPolyArea pts = sum $ zipWith (signedTriArea zero) pts (tail pts ++ [head pts])

-- | The (nonnegative) area of the polygon with the given vertices.
polyArea :: Fractional s => [P2 s] -> s
polyArea = abs . signedPolyArea
