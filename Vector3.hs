module Vector3 where

data Vec3 = Vec3
  { x :: Float
  , y :: Float
  , z :: Float
  } deriving (Show)

add :: Vec3 -> Vec3 -> Vec3
add (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Vec3 (x1 + x2) (y1 + y2) (z1 + z2)

sub :: Vec3 -> Vec3 -> Vec3
sub (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Vec3 (x1 - x2) (y1 - y2) (z1 - z2)

elementMul :: Vec3 -> Float -> Vec3
elementMul (Vec3 x y z) scala = Vec3 (x * scala) (y * scala) (z * scala)

dot :: Vec3 -> Vec3 -> Float
dot (Vec3 x y z) (Vec3 x1 y1 z1) = x * x1 + y * y1 + z * z1

lengthOf :: Vec3 -> Float
lengthOf (Vec3 x y z) = sqrt (x * x + y * y + z * z)

makeUnitVector :: Vec3 -> Vec3
makeUnitVector vec3 =
  let len = lengthOf vec3
   in elementMul vec3 (1 / len)
