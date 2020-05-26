module Util where

import           Data.Maybe

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

fj :: Maybe a -> a
fj = fromJust

both :: (a -> b) -> (a,a) -> (b,b)
both f (x,y) = (f x, f y)
