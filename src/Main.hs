{-
import qualified GI.Gtk as GI (main, init)

import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import GI.GdkPixbuf.Objects.Pixbuf

import GI.Gtk.Enums
       (Orientation(..), WindowType(..), ButtonBoxStyle(..))

import GI.Gtk.Objects.Image
       (imageNewFromFile)

import GI.Gtk
       (buttonBoxNew, buttonBoxSetChildSecondary, widgetShowAll,
        setButtonBoxLayoutStyle, buttonNewWithLabel, setContainerChild,
        setContainerBorderWidth, mainQuit, onWidgetDestroy, windowNew)

import qualified Data.Array.Repa as R

import Data.Array.Repa.Shape

main :: IO ()
main = do
  GI.init Nothing      
  window <- windowNew WindowTypeToplevel

  onWidgetDestroy window mainQuit

  setContainerBorderWidth window 10

  image <- imageNewFromFile "sample/lena-rgb.png"

  pbuf <- pixbufNewFromFile "sample/lena-rgb.png"

  nChannels <- pixbufGetNChannels pbuf
  width <- pixbufGetWidth pbuf
  height <- pixbufGetHeight pbuf

  print R.rank (R.Z R.:. (fromIntegral width :: Int) R.:. (fromIntegral height :: Int) R.:. (fromIntegral nChannels :: Int))

  setContainerChild window image
  widgetShowAll window  
  GI.main

getMtxShape :: Pixbuf -> sh
getMtxShape pix = (R.Z R.:. (fromIntegral pixbufGetWidth pix :: Int) R.:. (fromIntegral pixbufGetHeight pix :: Int) R.:. (fromIntegral pixbufGetNChannels pix :: Int))

-}

{-# LANGUAGE PackageImports, BangPatterns, QuasiQuotes, PatternGuards, ScopedTypeVariables, RecordWildCards #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-incomplete-patterns #-}

-- | Apply Sobel operators to an image.
import System.Environment
import Data.Array.Repa                          as R
import Data.Array.Repa.Algorithms.Pixel         as R
import Data.Array.Repa.IO.BMP
import Data.Array.Repa.IO.Timing
import Codec.Picture                            as C
import Prelude                          hiding (compare)
import Control.Monad
import Gradient
--import Converter
import Debug.Trace

main :: IO ()
main = do
  [path, path'] <- getArgs
  eimg <- readImage path
  case eimg of
    Left err -> putStrLn ("Could not read image: " Prelude.++ err)
    Right (ImageRGB8 img) -> do
      gray <- (R.computeUnboxedP . toGray . fromImage) img
      edges <- sobel gray
      --color <- R.computeUnboxedP $ toRGB gray
      (savePngImage path' . ImageY8 . toGrayImage) edges
    Right _ -> putStrLn "Unexpected pixel format"

type RGB8 = (Pixel8, Pixel8, Pixel8)

-- | Produce delayed Repa array from image with true color pixels.
fromImage :: Image PixelRGB8 -> Array D DIM2 RGB8
fromImage img@Image {..} =
  R.fromFunction
    (Z :. imageWidth :. imageHeight)
    (\(Z :. x :. y) ->
       let (PixelRGB8 r g b) = pixelAt img x y
       in (r, g, b))

-- | Get image with true color pixels from manifest Repa array.
toImage :: Array U DIM2 RGB8 -> Image PixelRGB8
toImage a = generateImage gen width height
  where
    Z :. width :. height = R.extent a
    gen x y =
      let (r,g,b) = a ! (Z :. x :. y)
      in PixelRGB8 r g b

-- | Get image with true color pixels from manifest Repa array.
toGrayImage :: Array U DIM2 Float -> Image Pixel8
toGrayImage a = generateImage gen width height
  where
    Z :. width :. height = R.extent a
    gen x y = round (255 * (a ! (Z :. x :. y)))

toRGB :: Array D DIM2 Float -> Array D DIM2 RGB8
toRGB = R.map (\x -> rgb8OfGreyFloat (x/3))

toGray :: Array D DIM2 RGB8 -> Array D DIM2 Float
toGray = R.map floatLuminanceOfRGB8

sobel :: Monad m => GImage -> m GImage
sobel img = do
       gX <- sobelX img 0
       gY <- sobelY img 0
       computeUnboxedP $ R.zipWith magnitude gX gY

-- | Determine the squared magnitude of a vector.
magnitude :: Float -> Float -> Float
{-# INLINE magnitude #-}
magnitude x y
        = sqrt (x * x + y * y)




{--

run iterations fileIn fileOut
 = do   -- Load the source image and convert it to greyscale
       traceEventIO "******** Sobel Read Image"

       eimg <- readImage path
       case eimg of
              Left err -> die ("Could not read image: " ++ err)
              Right (ImageRGB8 img) -> loop 1 $ computeP $ R.map floatLuminanceOfRGB8 (fromImage img)
              Right _ -> die "Unexpected pixel format"

        inputImage      <- liftM (either (error . show) id) 
                        $ fromImage (readImage fileIn)

        traceEventIO "******** Sobel Luminance"
        (greyImage :: Array U DIM2 Float)
                        <- computeP
                        $  R.map floatLuminanceOfRGB8 inputImage
                
        -- Run the filter.
        traceEventIO "******** Sobel Loop Start"
        ((gX, gY), tElapsed)
                       <- time $ loop iterations greyImage

        traceEventIO "******** Sobel Loop End"
        putStr $ prettyTime tElapsed
        
        -- Write out the magnitute of the vector gradient as the result image.
        traceEventIO "******** Sobel Magnitude"
        outImage       <- computeUnboxedP
                       $  R.map rgb8OfGreyFloat  
                       $  R.map (/ 3)
                       $  R.zipWith magnitude gX gY     

        traceEventIO "******** Sobel Write Image"
        writeImageToBMP fileOut outImage

loop :: Int -> GImage -> IO (GImage, GImage)
loop n img
 = img `deepSeqArray`
   if n == 0
    then return (img, img)
    else do 
        traceEventIO $ "******** Sobel Loop " Prelude.++ show n
        gX      <- sobelX img 0
        gY      <- sobelX img 0       
        if (n == 1) 
                then return (gX, gY)
                else loop (n - 1) img


-- | Determine the squared magnitude of a vector.
magnitude :: Float -> Float -> Float
{-# INLINE magnitude #-}
magnitude x y
        = sqrt (x * x + y * y)

--}

