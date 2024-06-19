module Ray (
        Ray (..),
        origin,
        direction,
        at,
) where

import Vec3

type Point3 = Vec3

data Ray = Ray {orig :: Point3, dir :: Vec3}
        deriving (Show, Eq)

origin :: Ray -> Point3
origin (Ray o _) = o

direction :: Ray -> Vec3
direction (Ray _ d) = d

at :: Ray -> Double -> Point3
at (Ray o d) t = o + d `scalarMult` t
