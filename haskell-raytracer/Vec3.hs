module Vec3
  ( Vec3 (..),
    dot,
    cross,
    lengthVec,
    lengthSquared,
    nearZero,
    scalarMult,
    scalarDiv,
    unitVector,
    randomVec3,
    randomRangeVec3,
    randomRangeDisk,
    randomInUnitSphere,
    randomUnitVec3,
    randomOnHemisphere,
    randomInUnitDisk,
    reflect,
    refract,
  )
where

import Distribution.Verbosity (normal)
import Stuff

-- Vector data type
data Vec3 = Vec3 {x :: Double, y :: Double, z :: Double}
  deriving (Show, Eq)

type Point3 = Vec3

instance Num Vec3 where
  (Vec3 x1 y1 z1) + (Vec3 x2 y2 z2) = Vec3 (x1 + x2) (y1 + y2) (z1 + z2)
  (Vec3 x1 y1 z1) - (Vec3 x2 y2 z2) = Vec3 (x1 - x2) (y1 - y2) (z1 - z2)
  (Vec3 x1 y1 z1) * (Vec3 x2 y2 z2) = Vec3 (x1 * x2) (y1 * y2) (z1 * z2)
  negate (Vec3 x y z) = Vec3 (-x) (-y) (-z)
  abs (Vec3 x y z) = Vec3 (abs x) (abs y) (abs z)
  signum (Vec3 x y z) = Vec3 (signum x) (signum y) (signum z)
  fromInteger n = Vec3 (fromInteger n) (fromInteger n) (fromInteger n)

dot :: Vec3 -> Vec3 -> Double
dot (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = x1 * x2 + y1 * y2 + z1 * z2

cross :: Vec3 -> Vec3 -> Vec3
cross (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) =
  Vec3
    (y1 * z2 - z1 * y2)
    (z1 * x2 - x1 * z2)
    (x1 * y2 - y1 * x2)

lengthVec :: Vec3 -> Double
lengthVec v = sqrt (lengthSquared v)

lengthSquared :: Vec3 -> Double
lengthSquared (Vec3 x y z) = x * x + y * y + z * z

epsilon :: Double
epsilon = 1e-8

nearZero :: Vec3 -> Bool
nearZero (Vec3 x y z) = abs x < epsilon && abs y < epsilon && abs z < epsilon

scalarMult :: Vec3 -> Double -> Vec3
scalarMult (Vec3 x y z) n = Vec3 (x * n) (y * n) (z * n)

scalarDiv :: Vec3 -> Double -> Vec3
scalarDiv vec n = scalarMult vec (1 / n)

unitVector :: Vec3 -> Vec3
unitVector v = scalarDiv v (lengthVec v)

randomVec3 :: IO Vec3
randomVec3 = do
  x <- randomDouble
  y <- randomDouble
  z <- randomDouble
  return $ Vec3 x y z

randomRangeVec3 :: Double -> Double -> IO Vec3
randomRangeVec3 min max = do
  x <- randomDoubleRange min max
  y <- randomDoubleRange min max
  z <- randomDoubleRange min max
  return $ Vec3 x y z

randomRangeDisk :: Double -> Double -> IO Vec3
randomRangeDisk min max = do
  x <- randomDoubleRange min max
  y <- randomDoubleRange min max
  return $ Vec3 x y 0

randomInUnitSphere :: IO Vec3
randomInUnitSphere = do
  p <- randomRangeVec3 (-1) 1
  if lengthSquared p < 1
    then return p
    else randomInUnitSphere

randomUnitVec3 :: IO Vec3
randomUnitVec3 = do unitVector <$> randomInUnitSphere

randomOnHemisphere :: Vec3 -> IO Vec3
randomOnHemisphere normal = do
  onUnitSphere <- randomUnitVec3
  if onUnitSphere `dot` normal > 0.0
    then return onUnitSphere
    else return $ negate onUnitSphere

randomInUnitDisk :: IO Vec3
randomInUnitDisk = do
  p <- randomRangeDisk (-1) 1
  if lengthSquared p < 1
    then return p
    else randomInUnitDisk

-- TODO: Figure out why reflect and refract are flipped
reflect :: Vec3 -> Vec3 -> Vec3
reflect v n = v - (n `scalarMult` (2 * (v `dot` n)))

refract :: Vec3 -> Vec3 -> Double -> Vec3
refract uv n etaiOveretat =
  let cosTheta = min ((-uv) `dot` n) 1.0
      rOutPerp = (uv + (n `scalarMult` cosTheta)) `scalarMult` etaiOveretat
      rOutParallel = n `scalarMult` (-(sqrt (abs (1.0 - lengthSquared rOutPerp))))
   in -(rOutPerp + rOutParallel)
