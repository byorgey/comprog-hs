{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Geom where

------------------------------------------------------------
-- 2D points and vectors

data V2 s = V2 !s !s deriving (Eq, Ord, Show)
type V2D  = V2 Double

instance Foldable V2 where
  foldMap f (V2 x y) = f x <> f y

zero :: Num s => V2 s
zero = V2 0 0

-- Adding and subtracting vectors
(^+^), (^-^) :: Num s => V2 s -> V2 s -> V2 s
V2 x1 y1 ^+^ V2 x2 y2 = V2 (x1+x2) (y1+y2)
V2 x1 y1 ^-^ V2 x2 y2 = V2 (x1-x2) (y1-y2)

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

-- Construct a vector in polar coordinates.
fromPolar :: Double -> Angle -> V2D
fromPolar r θ = rot θ (V2 r 0)

-- Rotate a vector counterclockwise by a given angle.
rot :: Angle -> V2D -> V2D
rot (A θ) (V2 x y) = V2 (cos θ * x - sin θ * y) (sin θ * x + cos θ * y)
