-- https://byorgey.wordpress.com/2020/06/24/competitive-programming-in-haskell-vectors-and-2d-geometry/

{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns               #-}

module Geom where

import           Data.Function (on)
import           Data.Ord      (compare)
import           Data.Ratio

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

-- | These combinators allows us to write e.g. 'v2 int' or 'v2 double'
--   to get a 'Scanner (V2 s)'.
v2, p2 :: Applicative f => f s -> f (V2 s)
v2 s = V2 <$> s <*> s
p2 = v2

newtype ByX s = ByX { unByX :: V2 s } deriving (Eq, Show, Functor)
newtype ByY s = ByY { unByY :: V2 s } deriving (Eq, Show, Functor)

instance Ord s => Ord (ByX s) where
  compare = compare `on` (getX . unByX)

instance Ord s => Ord (ByY s) where
  compare = compare `on` (getY . unByY)

------------------------------------------------------------
-- Angles

newtype Angle s = A s  -- angle (radians)
  deriving (Show, Eq, Ord, Num, Fractional, Floating)

fromDeg :: Floating s => s -> Angle s
fromDeg d = A (d * pi / 180)

fromRad :: s -> Angle s
fromRad = A

toDeg :: Floating s => Angle s -> s
toDeg (A r) = r * 180 / pi

toRad :: Angle s -> s
toRad (A r) = r

dir :: V2D -> Angle Double
dir (V2 x y) = A $ atan2 y x

-- | Construct a vector in polar coordinates.
fromPolar :: Floating s => s -> Angle s -> V2 s
fromPolar r θ = rot θ (V2 r 0)

-- | Rotate a vector counterclockwise by a given angle.
rot :: Floating s => Angle s -> V2 s -> V2 s
rot (A θ) (V2 x y) = V2 (cos θ * x - sin θ * y) (sin θ * x + cos θ * y)

perp :: Num s => V2 s -> V2 s
perp (V2 x y) = V2 (-y) x

------------------------------------------------------------
-- Dot product

-- | Dot product of two vectors.  u·v = |u||v| cos θ (where θ is the
--   (unsigned) angle between u and v).  So u·v is zero iff the vectors
--   are perpendicular.
dot :: Num s => V2 s -> V2 s -> s
dot (V2 x1 y1) (V2 x2 y2) = x1*x2 + y1*y2

-- | 'dotP p1 p2 p3' computes the dot product of the vectors from p1
-- to p2 and from p1 to p3.
dotP :: Num s => P2 s -> P2 s -> P2 s -> s
dotP p1 p2 p3 = dot (p2 ^-^ p1) (p3 ^-^ p1)

-- | Squared norm of a vector, /i.e./ square of its length, /i.e./ dot
--   product with itself.
normSq :: Num s => V2 s -> s
normSq v = dot v v

-- | Norm, /i.e./ length of a vector.
norm :: Floating s => V2 s -> s
norm = sqrt . normSq

-- | 'angleP p1 p2 p3' computes the (unsigned) angle of p1-p2-p3
--   (/i.e./ the angle at p2 made by rays to p1 and p3).  The result
--   will always be in the range $[0, \pi]$.
angleP :: Floating s => P2 s -> P2 s -> P2 s -> Angle s
angleP x y z = A $ acos (dot a b / (norm a * norm b))
  where
    a = (x ^-^ y)
    b = (z ^-^ y)

-- | 'signedAngleP p1 p2 p3' computes the /signed/ angle p1-p2-p3
--   (/i.e./ the angle at p2 made by rays to p1 and p3), in the range
--   $[-\pi, \pi]$.  Positive iff the ray from p2 to p3 is
--   counterclockwise of the ray from p2 to p1.
signedAngleP :: (Floating s, Ord s) => P2 s -> P2 s -> P2 s -> Angle s
signedAngleP x y z = case turnP x y z of
  CCW -> -angleP x y z
  _   -> angleP x y z

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

-- | Direction of a turn: counterclockwise (left), clockwise (right),
--   or parallel (/i.e./ 0 or 180 degree turn).
data Turn = CCW | Par | CW

-- | Cross product can also be used to compute the direction of a
--   turn.  If you are travelling from p1 to p2, 'turnP p1 p2 p3' says
--   whether you have to make a left (ccw) or right (cw) turn to
--   continue on to p3 (or if it is parallel).  Equivalently, if you
--   are standing at p1 looking towards p2, and imagine the line
--   through p1 and p2 dividing the plane in two, is p3 on the right
--   side, left side, or on the line?
turnP :: (Num s, Ord s) => P2 s -> P2 s -> P2 s -> Turn
turnP x y z
  | s > 0 = CCW
  | s == 0 = Par
  | otherwise = CW
  where
    s = signum (crossP x y z)

------------------------------------------------------------
-- 2D Lines

data L2 s = L2 { getDirection :: !(V2 s), getOffset :: !s }
type L2D = L2 Double

lineFromEquation :: Num s => s -> s -> s -> L2 s
lineFromEquation a b c = L2 (V2 b (-a)) c

lineFromPoints :: Num s => P2 s -> P2 s -> L2 s
lineFromPoints p q = L2 v (v `cross` p)
  where
    v = q ^-^ p

slope :: (Integral n, Eq n) => L2 n -> Maybe (Ratio n)
slope (getDirection -> V2 x y) = case x of
  0 -> Nothing
  _ -> Just (y % x)

side :: Num s => L2 s -> P2 s -> s
side (L2 v c) p = cross v p - c

leftOf :: (Num s, Ord s) => L2 s -> P2 s -> Bool
leftOf l p = side l p > 0

rightOf :: (Num s, Ord s) => L2 s -> P2 s -> Bool
rightOf l p = side l p < 0

toProjection :: Fractional s => L2 s -> P2 s -> V2 s
toProjection l@(L2 v _) p = (-side l p / normSq v) *^ perp v

project :: Fractional s => L2 s -> P2 s -> P2 s
project l p = p ^+^ toProjection l p

reflectAcross :: Fractional s => L2 s -> P2 s -> P2 s
reflectAcross l p = p ^+^ (2 *^ toProjection l p)
