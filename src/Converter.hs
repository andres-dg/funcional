{-# LANGUAGE PackageImports, BangPatterns, QuasiQuotes, PatternGuards, RecordWildCards #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-incomplete-patterns #-}

module Converter
(RGB8, fromImage, toImage)
where
import Data.Array.Repa                  as R
import Codec.Picture                    as C

type RGB8 = (Pixel8, Pixel8, Pixel8)
type MImage = Array D DIM2 RGB8

-- | Produce delayed Repa array from image with true color pixels.
fromImage :: Image PixelRGB8 -> MImage
fromImage img@Image {..} =
  R.fromFunction
    (Z :. imageWidth :. imageHeight)
    (\(Z :. x :. y) ->
       let (PixelRGB8 r g b) = pixelAt img x y
       in (r, g, b))

-- | Get image with true color pixels from manifest Repa array.
toImage :: MImage -> Image PixelRGB8
toImage a = generateImage gen width height
  where
    Z :. width :. height = R.extent a
    gen x y =
      let (r,g,b) = a ! (Z :. x :. y)
      in PixelRGB8 r g b