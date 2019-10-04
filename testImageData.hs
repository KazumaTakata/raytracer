import qualified ImageData

main = do
  let d = ImageData.RGBData []
  let d10 = ImageData.genNPixel d 20000
  let filePPM = ImageData.PPMFile 200 100 d10
  writeFile "sample.ppm" (ImageData.writePPM filePPM)
