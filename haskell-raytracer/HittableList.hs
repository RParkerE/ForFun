module HittableList (
        HittableList (..),
        addObject,
        clearHittableList,
) where

import Hittable
import Interval

newtype HittableList = HittableList {objects :: [HittableObject]}

instance Hittable HittableList where
        hit (HittableList objs) r (Interval tmin tmax) = foldl (hitFold r (Interval tmin tmax)) Nothing objs
            where
                hitFold ray (Interval tmin tmax) closestSoFar obj =
                        case hit obj ray (Interval tmin tmax) of
                                Nothing -> closestSoFar
                                Just tempRec -> case closestSoFar of
                                        Nothing -> Just tempRec
                                        Just closestRec ->
                                                if t tempRec < t closestRec
                                                        then Just tempRec{t = t tempRec, p = p tempRec, normal = normal tempRec, frontFace = frontFace tempRec}
                                                        else closestSoFar

addObject :: HittableList -> HittableObject -> HittableList
addObject (HittableList objects) obj = HittableList (obj : objects)

clearHittableList :: HittableList -> HittableList
clearHittableList _ = HittableList []
