module Camera
  ( Camera (..),
    render,
  )
where

import Color
import Control.Monad (foldM)
import Hittable
import HittableList
import Interval
import Ray
import Stuff
import System.IO
import Text.Printf (printf)
import Vec3
import Vec3 (lengthVec)

-- Define Point3 as Vec3
type Point3 = Vec3

-- Define the Camera data type
data Camera = Camera
  { aspectRatio :: Double, -- Ratio of image width over height
    imageWidth :: Int, -- Rendered image width in pixel count
    imageHeight :: Int, -- Rendered image height in pixel count
    center :: Point3, -- Camera center
    pixel00Loc :: Point3, -- Location of pixel (0,0)
    pixelDeltaU :: Vec3, -- Offset to pixel to the right
    pixelDeltaV :: Vec3, -- Offset to pixel below
    samplesPerPixel :: Int, -- Count of random samples for each pixel
    pixelSampleScale :: Double, -- Color scale factor for a sum of pixel samples
    maxDepth :: Int, -- Maximum number of ray bounces into scene
    vFOV :: Double, -- Vertical view angle (Field Of View)
    lookFromP3 :: Point3, -- Point camera is looking from
    lookAtP3 :: Point3, -- Point camera is looking at
    vUp :: Vec3, -- Camera-relative "up" direction
    defocusAngle :: Double, -- Variation angle of rays through each pixel
    focusDist :: Double, -- Distance from camera lookFromP3 point to plane of perfect focus
    defocusDiskU :: Vec3, -- Defocus disk horizontal radius
    defocusDiskV :: Vec3 -- Defocus disk vertical radius
  }

-- Initialize the camera parameters
initializeCam :: Camera -> Camera
initializeCam cam =
  let height = max 1 (round (fromIntegral (imageWidth cam) / aspectRatio cam))

      centerPoint = lookFromP3 cam

      theta = degreesToRadians (vFOV cam)
      h = tan (theta / 2)
      viewportHeight = 2 * h * focusDist cam
      viewportWidth = viewportHeight * (fromIntegral (imageWidth cam) / fromIntegral height)

      w = unitVector (lookFromP3 cam - lookAtP3 cam)
      u = unitVector (vUp cam `cross` w)
      v = w `cross` u

      viewportU = u `scalarMult` viewportWidth
      viewportV = (-v) `scalarMult` viewportHeight

      deltaU = viewportU `scalarDiv` fromIntegral (imageWidth cam)
      deltaV = viewportV `scalarDiv` fromIntegral height

      viewportUpperLeft = centerPoint - w `scalarMult` focusDist cam - viewportU `scalarDiv` 2 - viewportV `scalarDiv` 2
      pixel00 = viewportUpperLeft + (deltaU + deltaV) `scalarMult` 0.5

      defocusRadius = focusDist cam * tan (degreesToRadians (defocusAngle cam / 2))
   in cam
        { imageHeight = height,
          center = centerPoint,
          pixel00Loc = pixel00,
          pixelDeltaU = deltaU,
          pixelDeltaV = deltaV,
          pixelSampleScale = 1.0 / fromIntegral (samplesPerPixel cam),
          defocusDiskU = u `scalarMult` defocusRadius,
          defocusDiskV = v `scalarMult` defocusRadius
        }

render :: Handle -> Camera -> HittableList -> IO ()
render file cam world = do
  let initializedCam = initializeCam cam

  hPutStrLn file $ "P3\n" ++ show (imageWidth initializedCam) ++ " " ++ show (imageHeight initializedCam) ++ "\n255\n"

  -- Loop through each scanline (j)
  mapM_
    ( \j -> do
        putStr $ printf "\rScanlines remaining: %d " (imageHeight initializedCam - j)
        hFlush stdout -- Ensure the progress is printed immediately

        -- Loop through each pixel in the scanline (i)
        mapM_
          ( \i -> do
              -- Initialize pixel color to accumulate samples
              let initialColor = Vec3 0 0 0

              -- Accumulate colors for anti-aliasing
              pixelColor <-
                foldM
                  ( \acc _ -> do
                      -- Get a ray for this pixel sample
                      r <- getRay i j initializedCam

                      -- Calculate the color for this ray intersection with the world
                      color <- rayColor r (maxDepth cam) world

                      -- Accumulate the color
                      return $ acc + color
                  )
                  initialColor
                  [1 .. samplesPerPixel initializedCam]

              -- Compute the averaged color
              let averagedColor = pixelColor `scalarDiv` fromIntegral (samplesPerPixel initializedCam)

              -- Write the averaged color to the file
              writeColor file averagedColor
          )
          [0 .. imageWidth initializedCam - 1]
    )
    [0 .. imageHeight initializedCam - 1]

  putStrLn "\rDone."

sampleSquare :: IO Vec3
sampleSquare = do
  x <- randomDoubleRange (-0.5) 0.5
  y <- randomDoubleRange (-0.5) 0.5
  return $ Vec3 x y 0

getRay :: Int -> Int -> Camera -> IO Ray
getRay i j cam = do
  offset <- sampleSquare
  other <- defocusDiskSample cam
  let pixelSample =
        pixel00Loc cam
          + pixelDeltaU cam `scalarMult` (fromIntegral i + x offset)
          + pixelDeltaV cam `scalarMult` (fromIntegral j + y offset)
      rayOrigin =
        if defocusAngle cam <= 0
          then center cam
          else other
      rayDirection = pixelSample - rayOrigin
  return $ Ray rayOrigin rayDirection

defocusDiskSample :: Camera -> IO Point3
defocusDiskSample cam = do
  p <- randomInUnitDisk
  let scaledU = defocusDiskU cam `scalarMult` x p
      scaledV = defocusDiskV cam `scalarMult` y p
  return (center cam + scaledU + scaledV)

-- Compute the color of a ray based on intersection with hittable objects
rayColor :: Ray -> Int -> HittableList -> IO Color
rayColor _ 0 _ = return $ Vec3 0 0 0
rayColor r depth world = do
  let hitResult = hit world r (Interval 0.001 vInfinity)
  case hitResult of
    Nothing -> do
      let unitDirection = unitVector (direction r)
          t = 0.5 * (y unitDirection + 1.0)
          c1 = Vec3 1.0 1.0 1.0 `scalarMult` (1.0 - t)
          c2 = Vec3 0.5 0.7 1.0 `scalarMult` t
      return $ c1 + c2
    Just rec -> do
      scatterResult <- scatter (mat rec) r rec
      case scatterResult of
        Nothing -> return $ Vec3 0 0 0
        Just (scattered, attenuation) -> do
          color <- rayColor scattered (depth - 1) world
          return $ attenuation * color
