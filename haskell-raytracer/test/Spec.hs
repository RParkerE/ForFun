-- Haskell Implementation of Ray Tracing In One Weekend
-- Haskell Implementation by Robert Ellwanger (roberte3)
-- Original C++ Code by Peter Shirley, Trevor David Black, Steve Hollasch
-- https://raytracing.github.io/books/RayTracingInOneWeekend.html
module Main where

import Camera
import Color
import Control.Monad (forM)
import Hittable
import HittableList
import Interval
import Ray
import Sphere
import System.Directory (createDirectoryIfMissing, removeFile)
import System.IO (IOMode (WriteMode), hClose, hGetContents, openFile)
import Test.HUnit
import Text.Printf (printf)
import Vec3
import VectorConstants

createMyWorld :: IO HittableList
createMyWorld = do
  let groundMaterial = Material (Lambertian (Vec3 0.5 0.5 0.5))
      groundSphere = HittableObject (Sphere (Vec3 0 (-1000) 0) 1000 groundMaterial)

  spheres <-
    concat
      <$> forM
        [-11 .. 11]
        ( \a ->
            forM
              [-11 .. 11]
              ( \b -> do
                  chooseMat <- randomDouble
                  xOffset <- randomDouble
                  zOffset <- randomDouble
                  let center = Vec3 (fromIntegral a + 0.9 * xOffset) 0.2 (fromIntegral b + 0.9 * zOffset)
                  if vecLength (center - (Vec3 4 0.2 0)) > 0.9
                    then do
                      material <-
                        if chooseMat < 0.8
                          then Material . Lambertian <$> randomVec3
                          else
                            if chooseMat < 0.95
                              then do
                                albedo <- randomRangeVec3 0.5 1.0
                                fuzz <- randomDoubleRange 0.0 0.5
                                return (Material (Metal albedo fuzz))
                              else return (Material (Dielectric 1.5))
                      return [HittableObject (Sphere center 0.2 material)]
                    else return []
              )
        )

  let material1 = Material (Dielectric 1.5)
      sphere1 = HittableObject (Sphere (Vec3 0 1 0) 1.0 material1)
      material2 = Material (Lambertian (Vec3 0.4 0.2 0.1))
      sphere2 = HittableObject (Sphere (Vec3 (-4) 1 0) 1.0 material2)
      material3 = Material (Metal (Vec3 0.7 0.6 0.5) 0.0)
      sphere3 = HittableObject (Sphere (Vec3 4 1 0) 1.0 material3)

  return $ HittableList (groundSphere : sphere1 : sphere2 : sphere3 : concat spheres)

lowResCamera :: Camera
lowResCamera =
  Camera
    { aspectRatio = 16.0 / 9.0,
      imageW = 400,
      samplesPerPixel = 10,
      maxDepth = 5,
      vFOV = 20,
      lookFrom = Vec3 13 2 3,
      lookAt = Vec3 0 0 0,
      vUp = Vec3 0 1 0,
      defocusAngle = 0.6,
      focusDist = 10.0
    }

hiResCamera :: Camera
hiResCamera =
  Camera
    { aspectRatio = 16.0 / 9.0,
      imageW = 400,
      samplesPerPixel = 100,
      maxDepth = 50,
      vFOV = 20,
      lookFrom = Vec3 13 2 3,
      lookAt = Vec3 0 0 0,
      vUp = Vec3 0 1 0,
      defocusAngle = 0.6,
      focusDist = 10.0
    }

thetaCamera :: Camera
thetaCamera =
  Camera
    { aspectRatio = 16.0 / 9.0,
      imageW = 400,
      samplesPerPixel = 10,
      maxDepth = 5,
      vFOV = 20,
      lookFrom = Vec3 22 6 8,
      lookAt = Vec3 1 0 (-1),
      vUp = Vec3 0 1 0,
      defocusAngle = 0.6,
      focusDist = 10.0
    }

renderScene :: String -> HittableList -> Camera -> IO ()
renderScene outputPath scene camera = do
  handle <- openFile outputPath WriteMode
  renderWorld camera scene handle

testRenderLowResScene :: Test
testRenderLowResScene = TestCase $ do
  world <- createMyWorld
  createDirectoryIfMissing True "output"
  renderScene "output/low_res_scene.ppm" world lowResCamera

testRenderHighResScene :: Test
testRenderHighResScene = TestCase $ do
  world <- createMyWorld
  createDirectoryIfMissing True "output"
  renderScene "output/high_res_scene.ppm" world hiResCamera

testRenderNewViewScene :: Test
testRenderNewViewScene = TestCase $ do
  world <- createMyWorld
  createDirectoryIfMissing True "output"
  renderScene "output/angled_scene.ppm" world thetaCamera

-- Combine all tests
spec :: Test
spec =
  TestList
    [ TestLabel "Low Resolution Scene" testRenderLowResScene,
      TestLabel "High Resolution Scene" testRenderHighResScene,
      TestLabel "Different Viewing Angle Scene" testRenderNewViewScene
    ]

-- Run all tests
main :: IO ()
main = runTestTT spec >> return ()
