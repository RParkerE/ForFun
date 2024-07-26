module Ray
  ( -- Public Functions
    Ray (..),
    at,
  )
where

import Vec3

data Ray = Ray {origin :: Point3, direction :: Vec3}
  deriving (Show, Eq)

-- Calculate point along ray at given parameter t
at :: Ray -> Double -> Point3
at (Ray orig dir) t = orig + scalarMult dir t
