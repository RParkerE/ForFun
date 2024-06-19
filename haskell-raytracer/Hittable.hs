module Hittable
  ( Hittable (..),
    HitRecord (..),
    HittableObject (..),
    Material (..),
    Lambertian (..),
    Metal (..),
    Dielectric (..),
    scatter,
    scatterMaterial,
    setFaceNormal,
  )
where

import Color
import Interval
import Ray
import Stuff
import Vec3

data HitRecord = HitRecord {p :: Vec3, normal :: Vec3, mat :: Material, t :: Double, frontFace :: Bool}

-- class Materials m where
--  scatter :: m -> Ray -> HitRecord -> IO (Bool, Color, Ray)

class Materials m where
  scatter :: m -> Ray -> HitRecord -> IO (Maybe (Ray, Color))

-- Lambertian material
newtype Lambertian = Lambertian {albedo :: Color}

instance Materials Lambertian where
  scatter :: Lambertian -> Ray -> HitRecord -> IO (Maybe (Ray, Color))
  scatter (Lambertian albedo) rayIn rec = do
    randomDirection <- randomUnitVec3
    let scatterDirection = normal rec + randomDirection
        scatterDirection' = if nearZero scatterDirection then normal rec else scatterDirection
        scattered = Ray (p rec) scatterDirection'
    return $ Just (scattered, albedo)

-- Metal material
data Metal = Metal {element :: Color, fuzz :: Double}

instance Materials Metal where
  scatter :: Metal -> Ray -> HitRecord -> IO (Maybe (Ray, Color))
  scatter (Metal element fuzz) rayIn rec = do
    randomV <- randomVec3
    let fuzzy = min fuzz 1
        reflected = reflect (direction rayIn) (normal rec)
        reflected' = unitVector reflected + (randomV `scalarMult` fuzz)
        scattered = Ray (p rec) reflected'
    return $
      if direction scattered `dot` normal rec > 0
        then Just (scattered, element)
        else Nothing

-- Dielectric Material
newtype Dielectric = Dielectric {refractionIndex :: Double}

instance Materials Dielectric where
  scatter :: Dielectric -> Ray -> HitRecord -> IO (Maybe (Ray, Color))
  scatter (Dielectric refractionIndex) rayIn rec = do
    randDouble <- randomDouble
    let attenuation = Vec3 1 1 1
        ri = if (frontFace rec) then (1.0 / refractionIndex) else refractionIndex
        unitDirection = unitVector (direction rayIn)
        cosTheta = min ((-unitDirection) `dot` (normal rec)) 1.0
        sinTheta = sqrt (1.0 - cosTheta * cosTheta)
        cannotRefract = (ri * sinTheta) > 1.0
        directionRay =
          if (cannotRefract || (reflectance cosTheta ri) > randDouble)
            then reflect unitDirection (normal rec)
            else refract unitDirection (normal rec) ri
        scattered = Ray (p rec) directionRay
    return $ Just (scattered, attenuation)

reflectance :: Double -> Double -> Double
reflectance cosine ri =
  let r0 = (1 - ri) / (1 + ri)
      r0' = r0 * r0
   in r0' + (1 - r0') * ((1 - cosine) ** 5)

data Material = forall m. (Materials m) => Material m

instance Materials Material where
  scatter :: Material -> Ray -> HitRecord -> IO (Maybe (Ray, Color))
  scatter (Material m) rayIn rec = scatter m rayIn rec

scatterMaterial :: Material -> Ray -> HitRecord -> IO (Maybe (Ray, Color))
scatterMaterial (Material m) rayIn rec = scatter m rayIn rec

class Hittable h where
  hit :: h -> Ray -> Interval -> Maybe HitRecord

data HittableObject = forall h. (Hittable h) => HittableObject h

instance Hittable HittableObject where
  hit (HittableObject h) = hit h

setFaceNormal :: Ray -> Vec3 -> HitRecord -> HitRecord
setFaceNormal ray outwardNormal rec =
  let frontFace = direction ray `dot` outwardNormal < 0
      normal =
        if frontFace
          then outwardNormal
          else -outwardNormal
   in rec {frontFace = frontFace, normal = normal}
