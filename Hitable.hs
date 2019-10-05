module Hitable where

import ImageData
import Ray
import qualified Vector3

type Min = Float

type Max = Float

type Normal = Vector3.Vec3

type HitPoint = Vector3.Vec3

data Hit_record = Hit_record
  { t :: Float
  , normal :: Normal
  , hit_point :: HitPoint
  }

data HitRecordAndBool = HitRecordAndBool
  { hit_record :: Hit_record
  , isHit :: Bool
  }

class Hitable a where
  hit :: a -> Ray -> Min -> Max -> HitRecordAndBool

type Center = Vector3.Vec3

type Radius = Float

data Shpere =
  Shpere Center
         Radius

instance Hitable Shpere where
  hit (Shpere center radius) (Ray ori dir) min max =
    let oc = Vector3.sub ori center
        a = Vector3.dot dir dir
        b = Vector3.dot oc dir
        c = (Vector3.dot oc oc) - (radius * radius)
        disc = b * b - a * c
        dammy = Vector3.Vec3 0 0 0
        falsedata = HitRecordAndBool (Hit_record 0 dammy dammy) False
     in if (disc > 0)
          then let tmp = (-b - sqrt (disc)) / (a)
                in if (tmp < max) && (tmp > min)
                     then let point = pointAtParameter (Ray ori dir) tmp
                              normal =
                                Vector3.elementMul
                                  (Vector3.sub point center)
                                  (1 / radius)
                           in HitRecordAndBool
                                (Hit_record tmp normal point)
                                True
                     else let tmp = (-b + sqrt (disc)) / (a)
                           in if (tmp < max) && (tmp > min)
                                then let point =
                                           pointAtParameter (Ray ori dir) tmp
                                         normal =
                                           Vector3.elementMul
                                             (Vector3.sub point center)
                                             (1 / radius)
                                      in HitRecordAndBool
                                           (Hit_record tmp normal point)
                                           True
                                else falsedata
          else falsedata

data HitableList a =
  HitableList [a]

iterateHitable ::
     (Hitable a) => HitableList a -> Ray -> Min -> Max -> HitRecordAndBool
iterateHitable (HitableList (x:[])) ray min max = hit x ray min max
iterateHitable (HitableList (x:xs)) ray min max =
  let hitrecordandbool = hit x ray min max
      newmax =
        if isHit hitrecordandbool
          then t (hit_record hitrecordandbool)
          else max
      accHit = iterateHitable (HitableList xs) ray min newmax
   in if isHit accHit
        then accHit
        else hitrecordandbool
