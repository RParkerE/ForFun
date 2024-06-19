module Color (
        Color (..),
        writeColor,
) where

import Interval
import System.IO (Handle, hPutStr)
import Vec3

type Color = Vec3

writeColor :: Handle -> Color -> IO ()
writeColor fileHandler (Vec3 r g b) = do
        let myInterval = Interval 0.000 0.999
            rByte = round (255.999 * clamp myInterval (linearToGamma r))
            gByte = round (255.999 * clamp myInterval (linearToGamma g))
            bByte = round (255.999 * clamp myInterval (linearToGamma b))
        hPutStr fileHandler $ unwords [show rByte, show gByte, show bByte] ++ "\n"

linearToGamma :: Double -> Double
linearToGamma linearComponent =
        if linearComponent > 0
                then sqrt linearComponent
                else 0
