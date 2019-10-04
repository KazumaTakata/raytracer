module Main where

import ImageData
import Ray
import qualified Vector3

type Origin = Vector3.Vec3

type LowerLeft = Vector3.Vec3

type Horizontal = Vector3.Vec3

type Vertical = Vector3.Vec3

data Screen =
  Screen LowerLeft
         Horizontal
         Vertical

hitSphere :: Vector3.Vec3 -> Float -> Ray -> Float
hitSphere center radius (Ray ori dir) =
  let oc = Vector3.sub ori center
      a = Vector3.dot dir dir
      b = 2.0 * Vector3.dot oc dir
      c = (Vector3.dot oc oc) - (radius * radius)
      disc = b * b - 4 * a * c
   in if (disc < 0)
        then -1
        else (-b - sqrt (disc)) / (2 * a)

getColor :: Ray -> RGB
getColor (Ray ori dir) =
  let unit_dir = Vector3.makeUnitVector dir
      dir_y = 0.5 * ((Vector3.y unit_dir) + 1.0)
      center = Vector3.Vec3 0 0 (-1)
      t = hitSphere center 0.5 (Ray ori dir)
      vec3 =
        if t > 0
          then let n =
                     Vector3.makeUnitVector
                       (Vector3.sub
                          (pointAtParameter (Ray ori dir) t)
                          (Vector3.Vec3 0 0 (-1)))
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

renderAt :: X -> Y -> RGBData -> Origin -> Screen -> RGBData
renderAt x y (RGBData list) ori screen =
  let u = (x / 200.0)
      v = (y / 100.0)
      dir = genDirection screen u v
      ray = Ray ori dir
      color = getColor ray
   in RGBData (list ++ [color])

renderCol :: Y -> RGBData -> Origin -> Screen -> RGBData
renderCol 0 rgbdata ori screen = renderRow 0 0 rgbdata ori screen
renderCol y rgbdata ori screen =
  let newrgbdata = renderRow 0 y rgbdata ori screen
   in renderCol (y - 1) newrgbdata ori screen

renderRow :: X -> Y -> RGBData -> Origin -> Screen -> RGBData
renderRow 199 y rgbdata ori screen = renderAt 199 y rgbdata ori screen
renderRow x y rgbdata ori screen =
  let newrgbdata = renderAt x y rgbdata ori screen
   in renderRow (x + 1) y newrgbdata ori screen

genDirection :: Screen -> Float -> Float -> Vector3.Vec3
genDirection (Screen low hor ver) u v =
  Vector3.add
    (Vector3.add low (Vector3.elementMul hor u))
    (Vector3.elementMul ver v)

genScreen :: Screen
genScreen =
  let lowerLeftCorner = Vector3.Vec3 (-2) (-1) (-1)
      horizontal = Vector3.Vec3 4 0 0
      vertical = Vector3.Vec3 0 2 0
   in Screen lowerLeftCorner horizontal vertical

main = do
  let origin = Vector3.Vec3 0 0 0
  let screen = genScreen
  let initrbgdata = RGBData []
  let rbgdata = renderCol 99 initrbgdata origin screen
  let ppmFile = PPMFile 200 100 rbgdata
  let imagedata = writePPM ppmFile
  writeFile "sample.ppm" imagedata
  putStrLn "hello, world"
