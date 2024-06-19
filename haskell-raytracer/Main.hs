module Main where

import Camera
import Color
import Control.Monad (forM)
import Hittable
import HittableList
import Ray
import Sphere
import Stuff
import System.IO (IOMode (WriteMode), hClose, openFile)
import Vec3

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
                  if lengthVec (center - (Vec3 4 0.2 0)) > 0.9
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

main :: IO ()
main = do
  -- File
  handle <- openFile "output.ppm" WriteMode

  -- World
  world <- createMyWorld

  -- Camera
  let cam =
        Camera
          { aspectRatio = (16.0 / 9.0),
            imageWidth = 640,
            samplesPerPixel = 10,
            maxDepth = 50,
            vFOV = 20,
            lookFromP3 = Vec3 13 2 3,
            lookAtP3 = Vec3 0 0 0,
            vUp = Vec3 0 1 0,
            defocusAngle = 0.6,
            focusDist = 10,
            imageHeight = 0, -- Will be initialized by `render` function
            center = Vec3 0 0 0,
            pixel00Loc = Vec3 0 0 0,
            pixelDeltaU = Vec3 0 0 0,
            pixelDeltaV = Vec3 0 0 0,
            pixelSampleScale = 0.0,
            defocusDiskU = Vec3 0 0 0,
            defocusDiskV = Vec3 0 0 0
          }

  -- Render scene
  render handle cam world

  -- Close the file handle
  hClose handle

  putStrLn "Image generation complete. Output saved to 'output.ppm'."
