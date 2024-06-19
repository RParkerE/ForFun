module Stuff (
        vInfinity,
        vPi,
        degreesToRadians,
        randomDouble,
        randomDoubleRange,
) where

import System.Random

vInfinity :: Double
vInfinity = 1.0 / 0.0

vPi :: Double
vPi = 3.1415926535897932385

degreesToRadians :: Double -> Double
degreesToRadians degrees = degrees * vPi / 180.0

randomDouble :: IO Double
randomDouble = randomRIO (0.0, 1.0)

randomDoubleRange :: Double -> Double -> IO Double
randomDoubleRange min max = do
        rand <- randomDouble
        return $ min + (max - min) * rand
