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

lengthOf :: Vec3 -> Float
lengthOf (Vec3 x y z) = sqrt (x * x + y * y + z * z)

makeUnitVector :: Vec3 -> Vec3
makeUnitVector vec3 =
  let len = lengthOf vec3
   in elementMul vec3 (1 / len)
