module ImageData where

data ImageData =
  ImageData Int
            Int
            RGBData

data RGB = RGB
  { r :: Integer
  , g :: Integer
  , b :: Integer
  }

instance Show RGB where
  show (RGB r g b) = show r ++ " " ++ show g ++ " " ++ show b ++ "\n"

data RGBData =
  RGBData [RGB]
  deriving (Show)

appendData :: RGBData -> RGB -> RGBData
appendData (RGBData list) rgb = RGBData (list ++ [rgb])

genNPixel :: (Num n, Eq n) => RGBData -> n -> RGBData
genNPixel (RGBData list) 0 = RGBData list
genNPixel (RGBData list) n =
  let rgb = RGB 23 1 100
   in genNPixel (RGBData (list ++ [rgb])) (n - 1)

dataLength (RGBData list) = length list

rgb_add :: RGB -> RGB -> RGB
rgb_add (RGB r g b) (RGB r1 g1 b1) = RGB (r + r1) (g + g1) (b + b1)

rgbdata_concat :: RGBData -> RGBData -> RGBData
rgbdata_concat (RGBData list1) (RGBData list2) = RGBData (list1 ++ list2)

rgbdata_sum :: RGBData -> RGB
rgbdata_sum (RGBData (x:[])) = x
rgbdata_sum (RGBData (x:xs)) = rgb_add x (rgbdata_sum (RGBData xs))

rgbdata_average :: RGBData -> RGBData
rgbdata_average rgbdata =
  let sum = (rgbdata_sum rgbdata)
      leng = dataLength rgbdata
      aveR = quot (r sum) (toInteger leng)
      aveG = quot (g sum) (toInteger leng)
      aveB = quot (b sum) (toInteger leng)
   in RGBData [(RGB aveR aveG aveB)]

toString :: RGBData -> String
toString (RGBData (x:[])) = show x
toString (RGBData (x:xs)) = show x ++ toString (RGBData xs)

type Height = Int

type Width = Int

data PPMFile =
  PPMFile Width
          Height
          RGBData

writePPM :: PPMFile -> String
writePPM (PPMFile width height rgbdata) =
  "P3\n" ++ show width ++ " " ++ show height ++ "\n255\n" ++ toString rgbdata
