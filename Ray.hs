module Ray where

import qualified Vector3

data Ray = Ray
  { origin :: Vector3.Vec3
  , direction :: Vector3.Vec3
  }

pointAtParameter :: Ray -> Float -> Vector3.Vec3
pointAtParameter (Ray ori dir) t = Vector3.add ori (Vector3.elementMul dir t)
