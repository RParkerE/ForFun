{-# LANGUAGE ExistentialQuantification #-}

module HittableList
  ( -- Public Functions
    HittableObject (..),
    HittableList (..),
    clearHittableList,
    addToHittableList,
  )
where

import Control.Monad (foldM)
import Hittable
import Interval
import Ray
import Vec3

-- Type wrapper for Objects that are an instance of Hittable
-- Learned this syntax for wrapper here:
-- https://stackoverflow.com/questions/12774056/haskell-list-of-instances-of-a-typeclass
data HittableObject = forall h. (Hittable h) => HittableObject h

instance Hittable HittableObject where
  hit (HittableObject h) = hit h

-- List of Hittable data structures which represents the 'world'
data HittableList = HittableList {objects :: [HittableObject]}

-- Clear all instances from the list
clearHittableList :: HittableList -> HittableList
clearHittableList _ = HittableList []

-- Add a Hittable instance to the list
addToHittableList :: HittableList -> HittableObject -> HittableList
addToHittableList (HittableList objs) obj = HittableList (obj : objs)

instance Hittable HittableList where
  hit (HittableList objs) r (Interval rTMin rTMax) rec = do
    -- Loop over each HittableObject in the HittableList
    -- Do this monadically so we can carry state
    -- Help here from ChatGPT
    (hitAnything, closestSoFar, finalRec) <-
      foldM
        -- The lambda here carris the state
        ( \(hitAnything, closestSoFar, tempRec) (HittableObject obj) -> do
            hitResult <- hit obj r (Interval rTMin closestSoFar) tempRec
            case hitResult of
              Nothing -> return (hitAnything, closestSoFar, tempRec)
              Just newRec -> return (True, t newRec, newRec)
        )
        (False, rTMax, rec)
        objs

    return $ case hitAnything of
      True -> Just finalRec
      False -> Nothing
