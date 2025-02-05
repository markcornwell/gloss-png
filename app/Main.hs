module Main (main) where

import Graphics.Gloss
import Codec.Picture
import Data.Vector.Storable (toList)
import Foreign.Marshal.Array (newArray)
import Foreign.ForeignPtr (newForeignPtr_)

-- | Detects the format of the loaded image
imageFormat :: DynamicImage -> String
imageFormat (ImageY8 _)      = "Grayscale 8-bit"
imageFormat (ImageYA8 _)     = "Grayscale 8-bit with Alpha"
imageFormat (ImageRGB8 _)    = "RGB 8-bit"
imageFormat (ImageRGBA8 _)   = "RGBA 8-bit"
imageFormat (ImageYCbCr8 _)  = "YCbCr 8-bit"
imageFormat (ImageCMYK8 _)   = "CMYK 8-bit"
imageFormat (ImageRGBA16 _)  = "RGBA 16-bit"  -- ✅ 16-bit detected
imageFormat _                = "Unknown"

-- | Converts an image to 8-bit RGBA format if necessary
convertToRGBA8 :: DynamicImage -> Image PixelRGBA8
convertToRGBA8 (ImageRGBA16 img) =
    pixelMap (\(PixelRGBA16 r g b a) ->
        PixelRGBA8 (fromIntegral (r `div` 256))
                   (fromIntegral (g `div` 256))
                   (fromIntegral (b `div` 256))
                   (fromIntegral (a `div` 256))) img
convertToRGBA8 (ImageRGBA8 img) = img  -- Already in correct format
convertToRGBA8 _ = error "Unsupported format. Convert the image to RGBA."

-- | Converts JuicyPixels Image to Gloss BitmapData
convertToBitmapData :: Image PixelRGBA8 -> IO BitmapData
convertToBitmapData img = do
    let w = imageWidth img
        h = imageHeight img
        pixelList = toList (imageData img)  -- Extract raw pixel data
    pixelArray <- newArray pixelList  -- Allocate array in memory
    pixelPtr <- newForeignPtr_ pixelArray  -- Wrap it in a ForeignPtr
    let format = BitmapFormat TopToBottom PxRGBA
    return (bitmapDataOfForeignPtr w h format pixelPtr False)

-- | Main function to load and display an image using Gloss
main :: IO ()
main = do
    let filePath = "assets/spritesheet.png"
    result <- readImage filePath
    case result of
        Left err -> putStrLn $ "Error loading image: " ++ err
        Right dynImg -> do
            putStrLn $ "Loaded image format: " ++ show (imageFormat dynImg)
            let imgRGBA8 = convertToRGBA8 dynImg  -- ✅ Ensure we always have RGBA8
            bitmapData <- convertToBitmapData imgRGBA8
            putStrLn "Converted image to BitmapData. Launching Gloss window..."
            display (InWindow "Sprite Test" (800, 600) (100, 100)) white (bitmap bitmapData)
