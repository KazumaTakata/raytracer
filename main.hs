module Main where

import Hitable
import ImageData
import Ray
import qualified Vector3

type Origin = Vector3.Vec3

type LowerLeft = Vector3.Vec3

type Horizontal = Vector3.Vec3

type Vertical = Vector3.Vec3

getColor :: (Hitable a) => Ray -> HitableList a -> RGB
getColor (Ray ori dir) hitablelist =
  let unit_dir = Vector3.makeUnitVector dir
      dir_y = 0.5 * ((Vector3.y unit_dir) + 1.0)
      hitrecordandbool = iterateHitable hitablelist (Ray ori dir) 0 200000
      vec3 =
        if isHit hitrecordandbool
          then let n = normal (hit_record hitrecordandbool)
                   nx = (Vector3.x n)
                   ny = (Vector3.y n)
                   nz = (Vector3.z n)
                in Vector3.elementMul
                     (Vector3.Vec3 (nx + 1) (ny + 1) (nz + 1))
                     0.5
          else Vector3.add
                 (Vector3.elementMul (Vector3.Vec3 1 1 1) (1.0 - dir_y))
                 (Vector3.elementMul (Vector3.Vec3 0.5 0.7 1.0) dir_y)
   in RGB
        (round (255 * (Vector3.x vec3)))
        (round (255 * (Vector3.y vec3)))
        (round (255 * (Vector3.z vec3)))

type X = Float

type Y = Float

renderAt ::
     (Hitable a) => X -> Y -> RGBData -> Camera -> HitableList a -> RGBData
renderAt x y (RGBData list) (Camera ori low hor ver) hitablelist =
  let u = (x / 200.0)
      v = (y / 100.0)
      dir = genDirection (Camera ori low hor ver) u v
      ray = Ray ori dir
      color = getColor ray hitablelist
   in RGBData (list ++ [color])

renderCol :: (Hitable a) => Y -> RGBData -> Camera -> HitableList a -> RGBData
renderCol 0 rgbdata camera hitablelist =
  renderRow 0 0 rgbdata camera hitablelist
renderCol y rgbdata camera hitablelist =
  let newrgbdata = renderRow 0 y rgbdata camera hitablelist
   in renderCol (y - 1) newrgbdata camera hitablelist

renderRow ::
     (Hitable a) => X -> Y -> RGBData -> Camera -> HitableList a -> RGBData
renderRow 199 y rgbdata camera hitablelist =
  renderAt 199 y rgbdata camera hitablelist
renderRow x y rgbdata camera hitablelist =
  let newrgbdata = renderAt x y rgbdata camera hitablelist
   in renderRow (x + 1) y newrgbdata camera hitablelist

genDirection :: Camera -> Float -> Float -> Vector3.Vec3
genDirection (Camera ori low hor ver) u v =
  Vector3.add
    (Vector3.add low (Vector3.elementMul hor u))
    (Vector3.elementMul ver v)

data Camera =
  Camera Origin
         LowerLeft
         Horizontal
         Vertical

main = do
  let origin = Vector3.Vec3 0 0 0
  let lowerLeftCorner = Vector3.Vec3 (-2) (-1) (-1)
      horizontal = Vector3.Vec3 4 0 0
      vertical = Vector3.Vec3 0 2 0
  let camera = Camera origin lowerLeftCorner horizontal vertical
  let initrbgdata = RGBData []
  let sphere1 = Shpere (Vector3.Vec3 0 0 (-1)) 0.5
  let sphere2 = Shpere (Vector3.Vec3 0 (-100.5) (-1)) 100
  let hitablelist = HitableList [sphere1, sphere2]
  let rbgdata = renderCol 99 initrbgdata camera hitablelist
  let ppmFile = PPMFile 200 100 rbgdata
  let imagedata = writePPM ppmFile
  writeFile "sample.ppm" imagedata
  putStrLn "hello, world"
