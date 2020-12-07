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
import Codec.Picture                            as C
import Prelude                          hiding (compare)
import Gradient
--import Converter

type RGB8 = (Pixel8, Pixel8, Pixel8)

main :: IO ()
main = do
  [path, path'] <- getArgs
  eimg <- readImage path
  let img = case eimg of Right (ImageRGB8 x) -> x
  gray <- loadGrayImage img
  result <- normalFilter gray 3 1

  (savePngImage path' . ImageY8 . toGrayImage) result

loadGrayImage :: Monad m => Image PixelRGB8 -> m GImage
loadGrayImage = (R.computeUnboxedP . toGray . fromImage)

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
       gX <- convolute img 0 1 sobelX
       gY <- convolute img 0 1 sobelY
       computeUnboxedP $ R.zipWith magnitude gX gY

normalFilter :: Monad m => GImage -> Int -> Float -> m GImage
normalFilter img size sigma = do
    res <- niceGaussian img 0 size sigma
    normalize res


-- | Determine the squared magnitude of a vector.
magnitude :: Float -> Float -> Float
{-# INLINE magnitude #-}
magnitude x y
        = sqrt (x * x + y * y)

