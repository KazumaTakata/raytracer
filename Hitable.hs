module Hitable where

import ImageData
import Ray
import qualified Vector3

type Min = Float

type Max = Float

type Normal = Vector3.Vec3

type HitPoint = Vector3.Vec3

data Hit_record =
  Hit_record Float
             Normal
             HitPoint

data HitRecordAndBool =
  HitRecordAndBool Hit_record
                   Bool

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
        b = 2.0 * Vector3.dot oc dir
        c = (Vector3.dot oc oc) - (radius * radius)
        disc = b * b - a * c
        dammy = Vector3.Vec3 0 0 0
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
                                else HitRecordAndBool
                                       (Hit_record 0 dammy dammy)
                                       False
          else HitRecordAndBool (Hit_record 0 dammy dammy) False

data HitableList a =
  HitableList [a]
              Int

iterateHitable :: (Hitable a) => HitableList a -> HitRecordAndBool
