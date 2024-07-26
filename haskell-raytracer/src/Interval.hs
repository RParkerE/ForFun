module Interval
  ( -- Public Function
    Interval (..),
    iSize,
    iContains,
    iSurrounds,
    iEmpty,
    iUniverse,
    iClamp,
  )
where

import VectorConstants

data Interval = Interval {iMin :: Double, iMax :: Double}
  deriving (Show, Eq)

iSize :: Interval -> Double
iSize (Interval s l) = l - s

iContains :: Interval -> Double -> Bool
iContains (Interval s l) x = (s <= x) && (x <= l)

iSurrounds :: Interval -> Double -> Bool
iSurrounds (Interval s l) x = (s < x) && (x < l)

iEmpty :: Interval
iEmpty = Interval vInf (-vInf)

iUniverse :: Interval
iUniverse = Interval (-vInf) vInf

iClamp :: Interval -> Double -> Double
iClamp (Interval s l) x
  | x < s = s
  | x > l = l
  | otherwise = x
