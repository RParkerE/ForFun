module Camera
  ( -- Public Functions
    Camera (..),
    renderWorld,
    getRay,
    rayColor,
    initialize,
  )
where

import Color
import Control.Monad (foldM)
import GHC.IO.Handle (Handle, hPutStr)
import Hittable
import HittableList
import Interval
import Ray
import System.IO (hFlush, stdout)
import Text.Printf (printf)
import Vec3
import Vec3 (scalarDiv, scalarMult, unitVector, vecLength)
import VectorConstants
import VectorConstants (degreesToRadians)

-- Camera Data Structure and parameters needed for rendering
-- Only need to pass aspectRatio, imageW, samplesPerPixel, maxDepth,
-- vFOV, lookFrom, lookAt, defocusAngle, focusDist.
-- The rest of the parameters will be calculated and a fully
-- initialized camera is returned on initialize call
data Camera = Camera
  { aspectRatio :: Double,
    imageW :: Int,
    imageH :: Int,
    cameraCenter :: Point3,
    pixel00Loc :: Point3,
    pixelDeltaU :: Vec3,
    pixelDeltaV :: Vec3,
    samplesPerPixel :: Int,
    pixelSampleScale :: Double,
    maxDepth :: Int,
    vFOV :: Double,
    lookFrom :: Point3,
    lookAt :: Point3,
    vUp :: Vec3,
    defocusAngle :: Double,
    focusDist :: Double,
    defocusDiskU :: Vec3,
    defocusDiskV :: Vec3
  }
  deriving (Show, Eq)

-- Calculate the color of a ray as it interacts with the world
rayColor :: Camera -> Ray -> Int -> HittableList -> IO Color
rayColor cam r depth world
  | depth <= 0 = return $ Vec3 0 0 0
  | otherwise = do
      let rec = HitRecord (Vec3 0 0 0) (Vec3 0 0 0) (Material (Lambertian (Vec3 0 0 0))) 0 False
      hitResult <- hit world r (Interval 0.001 vInf) rec
      case hitResult of
        Just rec' -> do
          scatterResult <- scatter (mat rec') r rec'
          case scatterResult of
            Just (scattered, attenuation) -> do
              color <- rayColor cam scattered (depth - 1) world
              return $ attenuation * color
            Nothing -> return $ Vec3 0 0 0
        Nothing -> do
          let unitDirection = unitVector (direction r)
              t = 0.5 * (y unitDirection + 1.0)
          return $ scalarMult (Vec3 1.0 1.0 1.0) (1.0 - t) + scalarMult (Vec3 0.5 0.7 1.0) t

-- Calculate the "hidden"/"derived" parameters and return a fully initialized Camera
initialize :: Camera -> Camera
initialize cam =
  let height = max (floor (fromIntegral (imageW cam) / (aspectRatio cam))) 1 :: Int

      pSS = 1.0 / fromIntegral (samplesPerPixel cam)
      center = lookFrom cam

      theta = degreesToRadians (vFOV cam)
      h = tan (theta / 2)
      viewportH = 2 * h * (focusDist cam)
      viewportW = viewportH * (aspectRatio cam)

      w = unitVector (lookFrom cam - lookAt cam)
      u = unitVector (cross (vUp cam) w)
      v = cross w u

      viewportU = scalarMult u viewportW
      viewportV = scalarMult (-v) viewportH

      pDU = scalarDiv viewportU (fromIntegral (imageW cam))
      pDV = scalarDiv viewportV (fromIntegral height)

      viewportUpperLeft = center - (scalarMult w (focusDist cam)) - scalarDiv viewportU 2 - scalarDiv viewportV 2
      initLoc = viewportUpperLeft + scalarMult (pDU + pDV) 0.5

      defocusRad = (focusDist cam) * (tan (degreesToRadians ((defocusAngle cam) / 2)))
      defocusDU = scalarMult u defocusRad
      defocusDV = scalarMult v defocusRad
   in cam {imageH = height, cameraCenter = center, pixelDeltaU = pDU, pixelDeltaV = pDV, pixel00Loc = initLoc, pixelSampleScale = pSS, defocusDiskU = defocusDU, defocusDiskV = defocusDV}

-- Generate a Ray for a given pixel
getRay :: Camera -> Int -> Int -> IO Ray
getRay cam i j = do
  offset <- sampleSquare
  elseCon <- defocusDiskSample cam
  let pixelSample = pixel00Loc cam + (scalarMult (pixelDeltaU cam) (fromIntegral (i) + x offset)) + (scalarMult (pixelDeltaV cam) (fromIntegral (j) + y offset))
      rayOrigin = if (defocusAngle cam) <= 0 then cameraCenter cam else elseCon
      rayDirection = pixelSample - rayOrigin
  return $ Ray rayOrigin rayDirection

-- Generate random points within a unit square
sampleSquare :: IO Vec3
sampleSquare = do
  x <- randomDoubleRange (-0.5) 0.5
  y <- randomDoubleRange (-0.5) 0.5
  return $ Vec3 x y 0

-- Generate a random point on a unit disk (defocus disk)
defocusDiskSample :: Camera -> IO Point3
defocusDiskSample cam = do
  p <- randomInUnitDisk
  return $ (cameraCenter cam) + (scalarMult (defocusDiskU cam) (x p)) + (scalarMult (defocusDiskV cam) (y p))

-- Render Loop
renderWorld :: Camera -> HittableList -> Handle -> IO ()
renderWorld initCam world filename = do
  let cam = initialize initCam
  hPutStr filename $ "P3\n" ++ show (imageW cam) ++ " " ++ show (imageH cam) ++ "\n255\n"

  -- for(j = 0; j < imageH; j++)
  mapM_
    ( \j -> do
        putStr $ printf "\rScanlines remaining: %d " (imageH cam - j)
        hFlush stdout
        -- for(i = 0; i < imageW; i++)
        mapM_
          ( \i -> do
              pixelColor <-
                foldM
                  ( \acc _ -> do
                      r <- getRay cam i j
                      color <- rayColor cam r (maxDepth cam) world
                      return $ acc + color
                  )
                  (Vec3 0 0 0)
                  [1 .. samplesPerPixel cam]
              let finalColor = scalarMult pixelColor (pixelSampleScale cam)
              writeColor filename finalColor
          )
          [0 .. imageW cam - 1]
    )
    [0 .. imageH cam - 1]

  putStrLn "\rDone.                 "
