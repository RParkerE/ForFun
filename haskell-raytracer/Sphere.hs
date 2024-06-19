module Sphere
  ( Sphere (..),
  )
where

import Hittable
import Interval
import Ray
import Vec3

type Point3 = Vec3

data Sphere = Sphere {sphereCenter :: Point3, sphereRadius :: Double, sphereMaterial :: Material}

instance Hittable Sphere where
  hit (Sphere center radius mat) r (Interval tmin tmax) =
    let oc = origin r - center
        a = lengthSquared (direction r)
        h = dot (direction r) oc
        c = lengthSquared oc - radius * radius
        discriminant = h * h - a * c
     in if discriminant < 0
          then Nothing
          else
            let sqrtd = sqrt discriminant
                root1 = (-h - sqrtd) / a
                root2 = (-h + sqrtd) / a
                validRoot root = root > tmin && root < tmax
             in if validRoot root1
                  then Just $ makeHitRecord r root1 (center - at r root1 `scalarDiv` radius) mat
                  else
                    if validRoot root2
                      then Just $ makeHitRecord r root2 (center - at r root2 `scalarDiv` radius) mat
                      else Nothing

makeHitRecord :: Ray -> Double -> Vec3 -> Material -> HitRecord
makeHitRecord r t normal m =
  let p = at r t
      outwardNormal = unitVector normal
      frontFace = dot (direction r) outwardNormal < 0
      hitNormal = if frontFace then outwardNormal else negate outwardNormal
   in HitRecord p hitNormal m t frontFace

center :: Sphere -> Point3
center (Sphere c _ _) = c

radius :: Sphere -> Double
radius (Sphere _ r _) = r
