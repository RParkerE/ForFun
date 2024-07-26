module Vec3
  ( -- All public functions go here
    Vec3 (..),
    Point3,
    vecLength,
    vecLengthSquared,
    dot,
    cross,
    scalarMult,
    scalarDiv,
    unitVector,
    randomVec3,
    randomRangeVec3,
    randomInUnitSphere,
    randomUnitVec3,
    randomOnHemisphere,
    randomRangeDisk,
    randomInUnitDisk,
    nearZero,
    reflect,
    refract,
  )
where

import VectorConstants

data Vec3 = Vec3 {x :: Double, y :: Double, z :: Double}
  deriving (Show, Eq)

type Point3 = Vec3

-- ChatGPT offered this advice so I did not have to re-implement how math works
-- These are the // Vector Utility Functions from Ray Tracing In One Weekend
instance Num Vec3 where
  (Vec3 x1 y1 z1) + (Vec3 x2 y2 z2) = Vec3 (x1 + x2) (y1 + y2) (z1 + z2)
  (Vec3 x1 y1 z1) - (Vec3 x2 y2 z2) = Vec3 (x1 - x2) (y1 - y2) (z1 - z2)
  (Vec3 x1 y1 z1) * (Vec3 x2 y2 z2) = Vec3 (x1 * x2) (y1 * y2) (z1 * z2)
  negate (Vec3 x y z) = Vec3 (-x) (-y) (-z)
  abs (Vec3 x y z) = Vec3 (abs x) (abs y) (abs z)
  signum (Vec3 x y z) = Vec3 (signum x) (signum y) (signum z)
  fromInteger n = Vec3 (fromInteger n) (fromInteger n) (fromInteger n)

vecLength :: Vec3 -> Double
vecLength v = sqrt (vecLengthSquared v)

vecLengthSquared :: Vec3 -> Double
vecLengthSquared (Vec3 x y z) = x * x + y * y + z * z

dot :: Vec3 -> Vec3 -> Double
dot (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = x1 * x2 + y1 * y2 + z1 * z2

cross :: Vec3 -> Vec3 -> Vec3
cross (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) =
  Vec3
    (y1 * z2 - z1 * y2)
    (z1 * x2 - x1 * z2)
    (x1 * y2 - y1 * x2)

scalarMult :: Vec3 -> Double -> Vec3
scalarMult (Vec3 x y z) n = Vec3 (x * n) (y * n) (z * n)

scalarDiv :: Vec3 -> Double -> Vec3
scalarDiv v n = scalarMult v (1 / n)

unitVector :: Vec3 -> Vec3
unitVector v = scalarDiv v (vecLength v)

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

randomInUnitSphere :: IO Vec3
randomInUnitSphere = do
  p <- randomRangeVec3 (-1) 1
  if vecLengthSquared p < 1
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

randomRangeDisk :: Double -> Double -> IO Vec3
randomRangeDisk min max = do
  x <- randomDoubleRange min max
  y <- randomDoubleRange min max
  return $ Vec3 x y 0

randomInUnitDisk :: IO Vec3
randomInUnitDisk = do
  p <- randomRangeDisk (-1) 1
  if vecLengthSquared p < 1
    then return p
    else randomInUnitDisk

nearZero :: Vec3 -> Bool
nearZero (Vec3 x y z) =
  let s = 0.00000001
   in ((abs x) < s) && ((abs y) < s) && ((abs z) < s)

reflect :: Vec3 -> Vec3 -> Vec3
reflect v n = v - scalarMult n (2 * dot v n)

refract :: Vec3 -> Vec3 -> Double -> Vec3
refract uv n etaiIetat =
  let cosTheta = min (dot (-uv) n) 1.0
      rOutPerp = scalarMult (uv + scalarMult n cosTheta) etaiIetat
      rOutParallel = scalarMult n (-sqrt (abs (1.0 - vecLengthSquared rOutPerp)))
   in rOutPerp + rOutParallel
