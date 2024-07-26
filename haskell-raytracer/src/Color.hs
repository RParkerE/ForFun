module Color
  ( -- Public Functions
    Color (..),
    writeColor,
  )
where

import Interval
import System.IO (Handle, hPutStrLn)
import Vec3

type Color = Vec3

linearToGamma :: Double -> Double
linearToGamma linearComponent
  | linearComponent > 0 = sqrt linearComponent
  | otherwise = 0

writeColor :: Handle -> Color -> IO ()
writeColor filename pixelColor = do
  let r = linearToGamma (x pixelColor)
      g = linearToGamma (y pixelColor)
      b = linearToGamma (z pixelColor)

      i = Interval 0.000 0.999

      rbyte = round (255.999 * (iClamp i r))
      gbyte = round (255.999 * (iClamp i g))
      bbyte = round (255.999 * (iClamp i b))

  hPutStrLn filename $ show rbyte ++ " " ++ show gbyte ++ " " ++ show bbyte ++ "\n"
