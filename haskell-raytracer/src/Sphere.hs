module Sphere
  ( -- Public Functions
    Sphere (..),
  )
where

import Hittable
import Interval
import Ray
import Vec3

data Sphere = Sphere {sphereCenter :: Point3, sphereRadius :: Double, sphereMat :: Material}

-- Add the hit function to the Sphere data structure with an instance of our Hittable class
instance Hittable Sphere where
  hit (Sphere cen rad mat) r (Interval rTMin rTMax) rec = do
    let oc = origin r - cen
        a = vecLengthSquared (direction r)
        h = dot (direction r) oc
        c = vecLengthSquared oc - rad * rad
        discriminant = h * h - a * c
    case () of
      _
        | discriminant < 0 -> return Nothing
        | otherwise ->
            let sqrtD = sqrt discriminant
                root1 = (-h - sqrtD) / a
                root2 = (-h + sqrtD) / a
             in case () of
                  _
                    | root1 > rTMin && root1 < rTMax ->
                        let p = at r root1
                            outwardNormal = scalarDiv (p - cen) rad
                         in return $ Just (setFaceNormal (rec {t = root1, p = p, mat = mat}) r outwardNormal)
                    | root2 > rTMin && root2 < rTMax ->
                        let p = at r root2
                            outwardNormal = scalarDiv (p - cen) rad
                         in return $ Just (setFaceNormal (rec {t = root2, p = p, mat = mat}) r outwardNormal)
                    | otherwise -> return Nothing
