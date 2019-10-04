module ImageData where

data ImageData =
  ImageData Int
            Int
            RGBData

data RGB =
  RGB Integer
      Integer
      Integer

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
