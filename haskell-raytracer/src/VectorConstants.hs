module VectorConstants
  ( -- Public Functions
    vInf,
    vPI,
    degreesToRadians,
    randomDouble,
    randomDoubleRange,
  )
where

import System.Random

vInf :: Double
vInf = 1 / 0

vPI :: Double
vPI = 3.1415926535897932385

degreesToRadians :: Double -> Double
degreesToRadians deg = deg * vPI / 180.0

randomDouble :: IO Double
randomDouble = randomRIO (0.0, 1.0)

randomDoubleRange :: Double -> Double -> IO Double
randomDoubleRange min max = do
  rand <- randomDouble
  return $ min + (max - min) * rand
