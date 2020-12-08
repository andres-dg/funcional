




{-# LANGUAGE PackageImports, BangPatterns, QuasiQuotes, PatternGuards, ScopedTypeVariables, RecordWildCards #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-incomplete-patterns #-}

module Graphic (Transform, transformImg, normalFilter, sobel)
where

import System.Environment
import Data.Array.Repa                          as R
import Data.Array.Repa.Algorithms.Pixel         as R
import Codec.Picture                            as C
import Prelude                          hiding (compare)
import Gradient
import Noise

type RGB8 = (Pixel8, Pixel8, Pixel8)

type Transform = FilePath -> FilePath -> (GImage -> IO GImage) -> IO ()

transformImg :: FilePath -> FilePath -> (GImage -> IO GImage) -> IO ()
transformImg input output transform = do
  eimg <- readImage input
  img <-  case eimg of 
            Right (ImageRGB8 x) -> loadGrayImageFromRGB x
            Right (ImageY8 y) -> loadGrayImage y

  result <- transform img
  normed <- normalize result

  (savePngImage output . ImageY8 . toGrayImage) normed

loadGrayImageFromRGB :: Monad m => Image PixelRGB8 -> m GImage
loadGrayImageFromRGB = (R.computeUnboxedP . toGray . fromRGBImage)

loadGrayImage :: Monad m => Image Pixel8 -> m GImage
loadGrayImage = (R.computeUnboxedP . R.map (\x -> fromIntegral x / 255) . fromImage)

-- | Produce delayed Repa array from image with true color pixels.
fromImage :: Image Pixel8 -> Array D DIM2 Pixel8
fromImage img@Image {..} =
  R.fromFunction
    (Z :. imageWidth :. imageHeight)
    (\(Z :. x :. y) -> pixelAt img x y)

-- | Produce delayed Repa array from image with true color pixels.
fromRGBImage :: Image PixelRGB8 -> Array D DIM2 RGB8
fromRGBImage img@Image {..} =
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

normalFilter :: Monad m => Int -> Float ->  GImage -> m GImage
normalFilter size sigma img = do
    res <- niceGaussian img 0 size sigma
    normalize res


-- | Determine the squared magnitude of a vector.
magnitude :: Float -> Float -> Float
{-# INLINE magnitude #-}
magnitude x y
        = sqrt (x * x + y * y)

