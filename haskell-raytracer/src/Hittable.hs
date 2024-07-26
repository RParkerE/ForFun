-- These were told to me by ChatGPT
-- Allows me to build a test suite rather than a single rendered scene
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE InstanceSigs #-}

module Hittable
  ( -- Public Functions
    HitRecord (..),
    setFaceNormal,
    Hittable (..),
    Materials (..),
    Material (..),
    Lambertian (..),
    Metal (..),
    Dielectric (..),
  )
where

import Color
import Interval
import Ray
import Vec3
import Vec3 (unitVector)
import VectorConstants (randomDouble)

-- Record of a ray-object instersection
data HitRecord = HitRecord {p :: Point3, normal :: Vec3, mat :: Material, t :: Double, frontFace :: Bool}

-- Set the normal vector for a HitRecord based on the Ray's direction
setFaceNormal :: HitRecord -> Ray -> Vec3 -> HitRecord
setFaceNormal h r outwardNormal =
  let ff = dot (direction r) outwardNormal < 0
      norm = if ff then outwardNormal else -outwardNormal
   in h {normal = norm}

-- Needs to be a Type Class so that all things that are Hittable can implement this
class Hittable h where
  hit :: h -> Ray -> Interval -> HitRecord -> IO (Maybe HitRecord)

-- Another TypeClass which defines how a Ray scatters
class Materials m where
  scatter :: m -> Ray -> HitRecord -> IO (Maybe (Ray, Color))

data Material = forall m. (Materials m) => Material m

instance Materials Material where
  scatter :: Material -> Ray -> HitRecord -> IO (Maybe (Ray, Color))
  scatter (Material m) rayIn rec = scatter m rayIn rec

data Lambertian = Lambertian {albedo :: Color}

instance Materials Lambertian where
  scatter :: Lambertian -> Ray -> HitRecord -> IO (Maybe (Ray, Color))
  scatter (Lambertian albedo) rayIn rec = do
    randomVec <- randomUnitVec3
    let scatterDir = normal rec + randomVec
        scatterDir' = if nearZero scatterDir then normal rec else scatterDir
        scattered = Ray (p rec) scatterDir
    return $ Just (scattered, albedo)

data Metal = Metal {attenuation :: Color, fuzz :: Double}

instance Materials Metal where
  scatter :: Metal -> Ray -> HitRecord -> IO (Maybe (Ray, Color))
  scatter (Metal attenuation fuzz) rayIn rec = do
    randomUV <- randomUnitVec3
    let reflected = reflect (direction rayIn) (normal rec)
        reflected' = unitVector reflected + (scalarMult randomUV fuzz)
        scattered = Ray (p rec) reflected'
    return $ Just (scattered, attenuation)

data Dielectric = Dielectric {refractionIndex :: Double}

instance Materials Dielectric where
  scatter :: Dielectric -> Ray -> HitRecord -> IO (Maybe (Ray, Color))
  scatter (Dielectric refractionIndex) rayIn rec = do
    randDbl <- randomDouble
    let attenuation = Vec3 1 1 1
        ri = if (frontFace rec) then (1.0 / refractionIndex) else refractionIndex

        unitDir = unitVector (direction rayIn)
        cosTheta = min (dot (-unitDir) (normal rec)) 1
        sinTheta = sqrt (1.0 - cosTheta * cosTheta)

        cannotRefract = ri * sinTheta > 1.0
        directionRay
          | cannotRefract || (reflectance cosTheta ri) > randDbl = reflect unitDir (normal rec)
          | otherwise = refract unitDir (normal rec) ri

        scattered = Ray (p rec) directionRay
    return $ Just (scattered, attenuation)

reflectance :: Double -> Double -> Double
reflectance cosine refractInd =
  let r0 = (1 - refractInd) / (1 + refractInd)
      r0' = r0 * r0
   in r0' + (1 - r0') * ((1 - cosine) ^^ 5)
