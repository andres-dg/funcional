
{-# LINE 2 "./Graphics/UI/Gtk/Gdk/Pixbuf.chs" #-}
{-# LANGUAGE OverloadedStrings #-}

{-# LINE 3 "./Graphics/UI/Gtk/Gdk/Pixbuf.chs" #-}
{-# LANGUAGE EmptyDataDecls #-}

{-# LINE 4 "./Graphics/UI/Gtk/Gdk/Pixbuf.chs" #-}
-- -*-haskell-*-
-- GIMP Toolkit (GTK) Pixbuf
--
-- Author : Vincenzo Ciancia, Axel Simon
--
-- Created: 26 March 2002
--
-- Copyright (C) 2002-2005 Axel Simon, Vincenzo Ciancia
--
-- This library is free software; you can redistribute it and/or
-- modify it under the terms of the GNU Lesser General Public
-- License as published by the Free Software Foundation; either
-- version 2.1 of the License, or (at your option) any later version.
--
-- This library is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
-- Lesser General Public License for more details.
--
-- TODO
--
-- if anybody writes an image manipulation program, do the checker board
-- functions: gdk_pixbuf_composite_color_simple and
-- gdk_pixbuf_composite_color. Moreover, do: pixbuf_saturate_and_pixelate
--
--
-- pixbuf loader
--
-- module interface
--
-- rendering function for Bitmaps and Pixmaps when the latter are added
--
-- |
-- Maintainer : gtk2hs-users@lists.sourceforge.net
-- Stability : provisional
-- Portability : portable (depends on GHC)
--
-- 'Pixbuf's are bitmap images in memory.
--
-- * A Pixbuf is used to represent images. It contains information
-- about the image's pixel data, its color space, bits per sample, width
-- and height, and the rowstride or number of bytes between rows.
--
-- * This module contains functions to scale and crop
-- 'Pixbuf's and to scale and crop a 'Pixbuf' and
-- compose the result with an existing image.
--
-- * 'Pixbuf's can be displayed on screen by either creating an 'Image' that
-- from the 'Pixbuf' or by rendering (part of) the 'Pixbuf' into a
-- vanilla widget like 'DrawWindow' using
-- 'Graphics.UI.Gtk.Gdk.Drawable.drawPixbuf'.
--
module Graphics.UI.Gtk.Gdk.Pixbuf (
-- * Class Hierarchy
-- |
-- @
-- | 'GObject'
-- | +----Pixbuf
-- @

-- * Types
  Pixbuf,
  PixbufClass,
  castToPixbuf, gTypePixbuf,
  toPixbuf,
  PixbufError(..),
  Colorspace(..),

-- * Constructors
  pixbufNew,
  pixbufNewFromData,
  pixbufNewFromFile,

  pixbufNewFromFileAtSize,


  pixbufNewFromFileAtScale,





  pixbufNewFromInline,
  InlineImage,
  pixbufNewSubpixbuf,
  pixbufNewFromXPMData,

-- * Methods
  pixbufGetColorSpace,
  pixbufGetNChannels,
  pixbufGetHasAlpha,
  pixbufGetBitsPerSample,
  PixbufData,
  pixbufGetPixels,
  pixbufGetWidth,
  pixbufGetHeight,
  pixbufGetRowstride,
  pixbufGetOption,
  ImageFormat,
  pixbufGetFormats,
  pixbufSave,
  pixbufCopy,
  InterpType(..),
  pixbufScaleSimple,
  pixbufScale,
  pixbufComposite,

  pixbufFlipHorizontally,
  pixbufFlipHorazontally,
  pixbufFlipVertically,
  pixbufRotateSimple,
  PixbufRotation(..),

  pixbufAddAlpha,
  pixbufCopyArea,
  pixbufFill,

  pixbufGetFromDrawable,

  pixbufRenderThresholdAlpha,
  pixbufRenderPixmapAndMaskForColormap

  ) where

import Control.Monad (liftM)
import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.GObject
import Graphics.UI.Gtk.Types
{-# LINE 133 "./Graphics/UI/Gtk/Gdk/Pixbuf.chs" #-}

import Graphics.UI.Gtk.General.Structs (Rectangle(..))

import System.Glib.GError (GError(..), GErrorClass(..), GErrorDomain,
                                propagateGError)
import Graphics.UI.Gtk.Gdk.PixbufData ( PixbufData, mkPixbufData )

import Graphics.UI.Gtk.Gdk.Pixmap (Bitmap)






{-# LINE 147 "./Graphics/UI/Gtk/Gdk/Pixbuf.chs" #-}

-- | Error codes for loading image files.
--
data PixbufError = PixbufErrorCorruptImage
                 | PixbufErrorInsufficientMemory
                 | PixbufErrorBadOption
                 | PixbufErrorUnknownType
                 | PixbufErrorUnsupportedOperation
                 | PixbufErrorFailed
                 | PixbufErrorIncompleteAnimation
                 deriving (Enum)

{-# LINE 151 "./Graphics/UI/Gtk/Gdk/Pixbuf.chs" #-}

-- | Enumerate all supported color spaces.
--
-- * Only RGB is supported right now.
--
data Colorspace = ColorspaceRgb
                deriving (Enum)

{-# LINE 157 "./Graphics/UI/Gtk/Gdk/Pixbuf.chs" #-}

-- | Queries the color space of a pixbuf.
--
pixbufGetColorSpace :: Pixbuf -> IO Colorspace
pixbufGetColorSpace pb = liftM (toEnum . fromIntegral) $
  (\(Pixbuf arg1) -> withForeignPtr arg1 $ \argPtr1 ->gdk_pixbuf_get_colorspace argPtr1) pb

-- | Queries the number of colors for each pixel.
--
-- * This function returns 3 for an RGB image without alpha (transparency)
-- channel, 4 for an RGB image with alpha channel.
--
pixbufGetNChannels :: Pixbuf -> IO Int
pixbufGetNChannels pb = liftM fromIntegral $
  (\(Pixbuf arg1) -> withForeignPtr arg1 $ \argPtr1 ->gdk_pixbuf_get_n_channels argPtr1) pb

-- | Query if the image has an alpha channel.
--
-- * The alpha channel determines the opaqueness of the pixel.
--
pixbufGetHasAlpha :: Pixbuf -> IO Bool
pixbufGetHasAlpha pb =
  liftM toBool $ (\(Pixbuf arg1) -> withForeignPtr arg1 $ \argPtr1 ->gdk_pixbuf_get_has_alpha argPtr1) pb

-- | Queries the number of bits for each color.
--
-- * Each pixel is has a number of cannels for each pixel, each channel
-- has this many bits.
--
pixbufGetBitsPerSample :: Pixbuf -> IO Int
pixbufGetBitsPerSample pb = liftM fromIntegral $
  (\(Pixbuf arg1) -> withForeignPtr arg1 $ \argPtr1 ->gdk_pixbuf_get_bits_per_sample argPtr1) pb

-- | Retrieve the internal array of raw image data.
--
-- * Image data in a pixbuf is stored in memory in uncompressed,
-- packed format. Rows in the image are stored top to bottom, and in each
-- row pixels are stored from left to right. There may be padding at the
-- end of a row. The "rowstride" value of a pixbuf, as returned by
-- 'pixbufGetRowstride', indicates the number of bytes between rows.
--
-- * The returned array is a flat representation of a three dimensional
-- array: x-coordiante, y-coordinate and several channels for each color.
-- The number of channels is usually 3 for plain RGB data or 4 for
-- RGB data with an alpha channel. To read or write a specific pixel
-- use the formula: @p = y * rowstride + x * nChannels@ for the pixel.
-- If the array contains bytes (or 'Word8's), @p+0@ is the red value,
-- @p+1@ green, @p+2@ blue and @p+3@ the alpha (transparency) channel
-- if present. If the alpha channel is present, the array can accessed
-- as an array over 'Word32' to modify a whole pixel at a time. See also
-- 'pixbufGetBitsPerSample' and 'pixbufGetNChannels'.
--
-- * Calling this function without explicitly giving it a type will often
-- lead to a compiler error since the type parameter @e@ is underspecified.
-- If this happens the function can be explicitly typed:
-- @pbData <- (pixbufGetPixels pb :: IO (PixbufData Int Word8))@
--
-- * If modifying an image through Haskell\'s array interface is not
-- fast enough, it is possible to use 'unsafeRead' and
-- 'unsafeWrite' which have the same type signatures
-- as 'readArray' and 'writeArray'.
-- Note that these are internal
-- functions that might change with GHC.
--
pixbufGetPixels :: Storable e => Pixbuf -> IO (PixbufData Int e)
pixbufGetPixels pb = do
  pixPtr_ <- (\(Pixbuf arg1) -> withForeignPtr arg1 $ \argPtr1 ->gdk_pixbuf_get_pixels argPtr1) pb
  chan <- pixbufGetNChannels pb
  bits <- pixbufGetBitsPerSample pb
  w <- pixbufGetWidth pb
  h <- pixbufGetHeight pb
  r <- pixbufGetRowstride pb
  let pixPtr = castPtr pixPtr_
  let bytes = (h-1)*r+w*((chan*bits+7) `div` 8)
  return (mkPixbufData pb pixPtr bytes)

-- | Queries the width of this image.
--
pixbufGetWidth :: Pixbuf -> IO Int
pixbufGetWidth pb = liftM fromIntegral $
  (\(Pixbuf arg1) -> withForeignPtr arg1 $ \argPtr1 ->gdk_pixbuf_get_width argPtr1) pb

-- | Queries the height of this image.
--
pixbufGetHeight :: Pixbuf -> IO Int
pixbufGetHeight pb = liftM fromIntegral $
  (\(Pixbuf arg1) -> withForeignPtr arg1 $ \argPtr1 ->gdk_pixbuf_get_height argPtr1) pb

-- | Queries the rowstride of this image.
--
-- * Queries the rowstride of a pixbuf, which is the number of bytes between
-- rows. Use this value to caculate the offset to a certain row.
--
pixbufGetRowstride :: Pixbuf -> IO Int
pixbufGetRowstride pb = liftM fromIntegral $
  (\(Pixbuf arg1) -> withForeignPtr arg1 $ \argPtr1 ->gdk_pixbuf_get_rowstride argPtr1) pb

-- | Returns an attribut of an image.
--
-- * Looks up if some information was stored under the @key@ when
-- this image was saved.
--
pixbufGetOption :: (GlibString string) => Pixbuf -> string -> IO (Maybe string)
pixbufGetOption pb key = withUTFString key $ \strPtr -> do
  resPtr <- (\(Pixbuf arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gdk_pixbuf_get_option argPtr1 arg2) pb strPtr
  if (resPtr==nullPtr) then return Nothing else
    liftM Just $ peekUTFString resPtr

-- helper functions
pixbufErrorDomain :: GErrorDomain
pixbufErrorDomain = gdk_pixbuf_error_quark
{-# LINE 268 "./Graphics/UI/Gtk/Gdk/Pixbuf.chs" #-}

instance GErrorClass PixbufError where
  gerrorDomain _ = pixbufErrorDomain


-- | Load an image synchonously.
--
-- * Use this function to load only small images as this call will block.
--
-- * If an error occurs, the function will throw an exception that can
-- be caught using e.g. 'System.Glib.GError.catchGErrorJust' and one of the
-- error codes in 'PixbufError'.
--
pixbufNewFromFile :: GlibFilePath fp => fp -> IO Pixbuf
pixbufNewFromFile fname =
  wrapNewGObject mkPixbuf $
  propagateGError $ \errPtrPtr ->
     withUTFFilePath fname $ \strPtr ->



     gdk_pixbuf_new_from_file
{-# LINE 290 "./Graphics/UI/Gtk/Gdk/Pixbuf.chs" #-}

    strPtr errPtrPtr


-- | Creates a new pixbuf by loading an image from a file. The file format is
-- detected automatically. The image will be scaled to fit in the requested
-- size, preserving the image's aspect ratio.
--
-- * If an error occurs, the function will throw an exception that can
-- be caught using e.g. 'System.Glib.GError.catchGErrorJust' and one of the
-- error codes in 'PixbufError'.
--
-- * Available since Gtk+ version 2.4
--
pixbufNewFromFileAtSize :: GlibString string => string -> Int -> Int -> IO Pixbuf
pixbufNewFromFileAtSize filename width height =
  wrapNewGObject mkPixbuf $
  propagateGError $ \errPtrPtr ->
    withUTFString filename $ \filenamePtr ->



    gdk_pixbuf_new_from_file_at_size
{-# LINE 313 "./Graphics/UI/Gtk/Gdk/Pixbuf.chs" #-}

    filenamePtr
    (fromIntegral width)
    (fromIntegral height)
    errPtrPtr



-- | Creates a new pixbuf by loading an image from a file. The file format is
-- detected automatically. The image will be scaled to fit in the requested
-- size, optionally preserving the image's aspect ratio.
--
-- When preserving the aspect ratio, a width of -1 will cause the image to be
-- scaled to the exact given height, and a height of -1 will cause the image to
-- be scaled to the exact given width. When not preserving aspect ratio, a width
-- or height of -1 means to not scale the image at all in that dimension.
-- Negative values for width and height are allowed since Gtk+ 2.8.
--
-- * If an error occurs, the function will throw an exception that can
-- be caught using e.g. 'System.Glib.GError.catchGErrorJust' and one of the
-- error codes in 'PixbufError'.
--
-- * Available since Gtk+ version 2.6
--
pixbufNewFromFileAtScale :: GlibString string
  => string -- ^ the name of the file
  -> Int -- ^ target width
  -> Int -- ^ target height
  -> Bool -- ^ whether to preserve the aspect ratio
  -> IO Pixbuf
pixbufNewFromFileAtScale filename width height preserveAspectRatio =
  wrapNewGObject mkPixbuf $
  propagateGError $ \errPtrPtr ->
    withUTFString filename $ \filenamePtr ->



    gdk_pixbuf_new_from_file_at_scale
{-# LINE 351 "./Graphics/UI/Gtk/Gdk/Pixbuf.chs" #-}

    filenamePtr
    (fromIntegral width)
    (fromIntegral height)
    (fromBool preserveAspectRatio)
    errPtrPtr
{-# LINE 397 "./Graphics/UI/Gtk/Gdk/Pixbuf.chs" #-}
-- | A string representing an image file format.
--
type ImageFormat = DefaultGlibString

-- constant pixbufGetFormats A list of valid image file formats.
--
pixbufGetFormats :: [ImageFormat]

pixbufGetFormats = ["png","bmp","wbmp", "gif","ico","ani","jpeg","pnm",
                    "ras","tiff","xpm","xbm","tga"]

-- | Save an image to disk.
--
-- * The function takes a list of key - value pairs to specify
-- either how an image is saved or to actually save this additional
-- data with the image. JPEG images can be saved with a \"quality\"
-- parameter; its value should be in the range [0,100]. Text chunks
-- can be attached to PNG images by specifying parameters of the form
-- \"tEXt::key\", where key is an ASCII string of length 1-79.
-- The values are Unicode strings.
--
-- * If an error occurs, the function will throw an exception that can
-- be caught using e.g. 'System.Glib.GError.catchGErrorJust' and one of the
-- error codes in 'PixbufError'.
--
pixbufSave :: (GlibString string, GlibFilePath fp) => Pixbuf -> fp -> ImageFormat -> [(string, string)] ->
              IO ()
pixbufSave pb fname iType options =
  let (keys, values) = unzip options in
  propagateGError $ \errPtrPtr ->
    withUTFFilePath fname $ \fnPtr ->
    withUTFString iType $ \tyPtr ->
    withUTFStringArray0 keys $ \keysPtr ->
    withUTFStringArray values $ \valuesPtr -> do



      (\(Pixbuf arg1) arg2 arg3 arg4 arg5 arg6 -> withForeignPtr arg1 $ \argPtr1 ->gdk_pixbuf_savev argPtr1 arg2 arg3 arg4 arg5 arg6)
{-# LINE 435 "./Graphics/UI/Gtk/Gdk/Pixbuf.chs" #-}

        pb fnPtr tyPtr keysPtr valuesPtr errPtrPtr
      return ()

-- | Create a new image in memory.
--
-- * Creates a new pixbuf structure and allocates a buffer for
-- it. Note that the buffer is not cleared initially.
--
-- * The boolean flag is true if the pixbuf should have an alpha
-- (transparency) channel. The next integer denotes the bits per
-- color sample, e.g. 8 bits per color for 2^24 colors. The last
-- two integers denote the width and height, respectively.
--
pixbufNew :: Colorspace -> Bool -> Int -> Int -> Int -> IO Pixbuf
pixbufNew colorspace hasAlpha bitsPerSample width height =
  wrapNewGObject mkPixbuf $
    gdk_pixbuf_new ((fromIntegral . fromEnum) colorspace)
      (fromBool hasAlpha) (fromIntegral bitsPerSample) (fromIntegral width)
      (fromIntegral height)

pixbufNewFromData :: Ptr CUChar -> Colorspace -> Bool -> Int -> Int -> Int -> Int -> IO Pixbuf
pixbufNewFromData imData cSpace hasAlpha bitsPerSample width height rowStride
  = wrapNewGObject mkPixbuf $
     gdk_pixbuf_new_from_data
{-# LINE 460 "./Graphics/UI/Gtk/Gdk/Pixbuf.chs" #-}
       imData
       (fromIntegral . fromEnum $ cSpace)
       (fromBool hasAlpha)
       (fromIntegral bitsPerSample)
       (fromIntegral width)
       (fromIntegral height)
       (fromIntegral rowStride)
       nullFunPtr nullPtr

-- | Create a new image from a string.
--
-- * Creates a new pixbuf from a string description.
--
pixbufNewFromXPMData :: GlibString string => [string] -> IO Pixbuf
pixbufNewFromXPMData s =
  withUTFStringArray0 s $ \strsPtr ->
    wrapNewGObject mkPixbuf $ gdk_pixbuf_new_from_xpm_data strsPtr

-- | A dymmy type for inline picture data.
--
-- * This dummy type is used to declare pointers to image data
-- that is embedded in the executable. See
-- 'pixbufNewFromInline' for an example.
--
data InlineImage

-- | Create a new image from a static pointer.
--
-- * Like 'pixbufNewFromXPMData', this function allows to
-- include images in the final binary program. The method used by this
-- function uses a binary representation and therefore needs less space
-- in the final executable. Save the image you want to include as
-- @png@ and run:
--
-- > @echo #include "my_image.h" > my_image.c
-- > gdk-pixbuf-csource --raw --extern --name=my_image myimage.png >> my_image.c
--
-- on it. Write a header file @my_image.h@ containing:
--
-- > #include <gdk/gdk.h>
-- > extern guint8 my_image[];
--
-- and save it in the current directory.
-- The created file can be compiled with:
--
-- > cc -c my_image.c `pkg-config --cflags gdk-2.0`
--
-- into an object file which must be linked into your Haskell program by
-- specifying @my_image.o@ and @\"-#include my_image.h\"@ on
-- the command line of GHC.
-- Within you application you delcare a pointer to this image:
--
-- > foreign label "my_image" myImage :: Ptr InlineImage
--
-- Calling 'pixbufNewFromInline' with this pointer will
-- return the image in the object file. Creating the C file with
-- the @--raw@ flag will result in a non-compressed image in the
-- object file. The advantage is that the picture will not be
-- copied when this function is called.
--
--
pixbufNewFromInline :: Ptr InlineImage -> IO Pixbuf
pixbufNewFromInline iPtr = alloca $ \errPtrPtr -> do
  pbPtr <- gdk_pixbuf_new_from_inline (-1) (castPtr iPtr)
    (fromBool False) (castPtr errPtrPtr)
  if pbPtr/=nullPtr then wrapNewGObject mkPixbuf (return pbPtr)
    else do
      errPtr <- peek errPtrPtr
      (GError dom code msg) <- peek errPtr
      error $ glibToString msg

-- | Create a restricted view of an image.
--
-- * This function returns a 'Pixbuf' object which shares
-- the image of the original one but only shows a part of it.
-- Modifying either buffer will affect the other.
--
-- * This function throw an exception if the requested bounds are invalid.
--
pixbufNewSubpixbuf :: Pixbuf -> Int -> Int -> Int -> Int -> IO Pixbuf
pixbufNewSubpixbuf pb srcX srcY height width =
  wrapNewGObject mkPixbuf $ do
    pbPtr <- (\(Pixbuf arg1) arg2 arg3 arg4 arg5 -> withForeignPtr arg1 $ \argPtr1 ->gdk_pixbuf_new_subpixbuf argPtr1 arg2 arg3 arg4 arg5) pb
      (fromIntegral srcX) (fromIntegral srcY)
      (fromIntegral height) (fromIntegral width)
    if pbPtr==nullPtr then error "pixbufNewSubpixbuf: invalid bounds"
      else return pbPtr

-- | Create a deep copy of an image.
--
pixbufCopy :: Pixbuf -> IO Pixbuf
pixbufCopy pb = wrapNewGObject mkPixbuf $ (\(Pixbuf arg1) -> withForeignPtr arg1 $ \argPtr1 ->gdk_pixbuf_copy argPtr1) pb


-- | How an image is scaled.
--
-- [@InterpNearest@] Nearest neighbor sampling; this is the
-- fastest and lowest quality mode. Quality is normally unacceptable when
-- scaling down, but may be OK when scaling up.
--
-- [@InterpTiles@] This is an accurate simulation of the
-- PostScript image operator without any interpolation enabled. Each
-- pixel is rendered as a tiny parallelogram of solid color, the edges of
-- which are implemented with antialiasing. It resembles nearest neighbor
-- for enlargement, and bilinear for reduction.
--
-- [@InterpBilinear@] Best quality\/speed balance; use this
-- mode by default. Bilinear interpolation. For enlargement, it is
-- equivalent to point-sampling the ideal bilinear-interpolated
-- image. For reduction, it is equivalent to laying down small tiles and
-- integrating over the coverage area.
--
-- [@InterpHyper@] This is the slowest and highest quality
-- reconstruction function. It is derived from the hyperbolic filters in
-- Wolberg's \"Digital Image Warping\", and is formally defined as the
-- hyperbolic-filter sampling the ideal hyperbolic-filter interpolated
-- image (the filter is designed to be idempotent for 1:1 pixel mapping).
--
data InterpType = InterpNearest
                | InterpTiles
                | InterpBilinear
                | InterpHyper
                deriving (Enum)

{-# LINE 579 "./Graphics/UI/Gtk/Gdk/Pixbuf.chs" #-}

-- | Scale an image.
--
-- * Creates a new 'Pixbuf' containing a copy of
-- @src@ scaled to the given measures. Leaves @src@
-- unaffected.
--
-- * @interp@ affects the quality and speed of the scaling function.
-- 'InterpNearest' is the fastest option but yields very poor quality
-- when scaling down. 'InterpBilinear' is a good trade-off between
-- speed and quality and should thus be used as a default.
--
pixbufScaleSimple ::
  Pixbuf -- ^ @src@ - the source image
  -> Int -- ^ @width@ - the target width
  -> Int -- ^ @height@ the target height
  -> InterpType -- ^ interpolation type
  -> IO Pixbuf
pixbufScaleSimple pb width height interp =
    wrapNewGObject mkPixbuf $ liftM castPtr $
        (\(Pixbuf arg1) arg2 arg3 arg4 -> withForeignPtr arg1 $ \argPtr1 ->gdk_pixbuf_scale_simple argPtr1 arg2 arg3 arg4) (toPixbuf pb)
        (fromIntegral width) (fromIntegral height)
        (fromIntegral $ fromEnum interp)

-- | Copy a scaled image part to another image.
--
-- * This function is the generic version of 'pixbufScaleSimple'. It scales
-- @src@ by @scaleX@ and @scaleY@ and translate the image by @offsetX@ and
-- @offsetY@. Whatever is in the intersection with the rectangle @destX@,
-- @destY@, @destWidth@, @destHeight@ will be rendered into @dest@.
--
-- * The rectangle in the destination is simply overwritten. Use
-- 'pixbufComposite' if you need to blend the source image onto the
-- destination.
--
pixbufScale ::
    Pixbuf -- ^ @src@ - the source pixbuf
 -> Pixbuf -- ^ @dest@ - the pixbuf into which to render the results
 -> Int -- ^ @destX@ - the left coordinate for region to render
 -> Int -- ^ @destY@ - the top coordinate for region to render
 -> Int -- ^ @destWidth@ - the width of the region to render
 -> Int -- ^ @destHeight@ - the height of the region to render
 -> Double -- ^ @offsetX@ - the offset in the X direction (currently
               -- rounded to an integer)
 -> Double -- ^ @offsetY@ - the offset in the Y direction
               -- (currently rounded to an integer)
 -> Double -- ^ @scaleX@ - the scale factor in the X direction
 -> Double -- ^ @scaleY@ - the scale factor in the Y direction
 -> InterpType -- ^ the interpolation type for the transformation.
 -> IO ()
pixbufScale src dest destX destY destWidth destHeight offsetX offsetY
  scaleX scaleY interp =
  (\(Pixbuf arg1) (Pixbuf arg2) arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gdk_pixbuf_scale argPtr1 argPtr2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11) src dest
    (fromIntegral destX) (fromIntegral destY)
    (fromIntegral destWidth) (fromIntegral destHeight)
    (realToFrac offsetX) (realToFrac offsetY)
    (realToFrac scaleX) (realToFrac scaleY)
    ((fromIntegral . fromEnum) interp)

-- | Blend a scaled image part onto another image.
--
-- * This function is similar to 'pixbufScale' but allows the
-- original image to \"shine through\". The @alpha@ value determines
-- how opaque the source image is. Passing @0@ is
-- equivalent to not calling this function at all, passing
-- @255@ has the
-- same effect as calling 'pixbufScale'.
--
pixbufComposite ::
     Pixbuf -- ^ @src@ - the source pixbuf
  -> Pixbuf -- ^ @dest@ - the pixbuf into which to render the results
  -> Int -- ^ @destX@ - the left coordinate for region to render
  -> Int -- ^ @destY@ - the top coordinate for region to render
  -> Int -- ^ @destWidth@ - the width of the region to render
  -> Int -- ^ @destHeight@ - the height of the region to render
  -> Double -- ^ @offsetX@ - the offset in the X direction (currently
                -- rounded to an integer)
  -> Double -- ^ @offsetY@ - the offset in the Y direction
                -- (currently rounded to an integer)
  -> Double -- ^ @scaleX@ - the scale factor in the X direction
  -> Double -- ^ @scaleY@ - the scale factor in the Y direction
  -> InterpType -- ^ the interpolation type for the transformation.
  -> Word8 -- ^ @alpha@ - the transparency
  -> IO ()
pixbufComposite src dest destX destY destWidth destHeight
  offsetX offsetY scaleX scaleY interp alpha =
  (\(Pixbuf arg1) (Pixbuf arg2) arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gdk_pixbuf_composite argPtr1 argPtr2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12) src dest
  (fromIntegral destX) (fromIntegral destY) (fromIntegral destWidth)
  (fromIntegral destHeight) (realToFrac offsetX) (realToFrac offsetY)
  (realToFrac scaleX) (realToFrac scaleY)
  ((fromIntegral . fromEnum) interp) (fromIntegral alpha)


-- | Flips a pixbuf horizontally and returns the result in a new pixbuf.
--
pixbufFlipHorizontally :: Pixbuf -> IO Pixbuf
pixbufFlipHorizontally self =
  wrapNewGObject mkPixbuf $
  (\(Pixbuf arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gdk_pixbuf_flip argPtr1 arg2)
{-# LINE 678 "./Graphics/UI/Gtk/Gdk/Pixbuf.chs" #-}
    self
    (fromBool True)
pixbufFlipHorazontally = pixbufFlipHorizontally

-- | Flips a pixbuf vertically and returns the result in a new pixbuf.
--
pixbufFlipVertically :: Pixbuf -> IO Pixbuf
pixbufFlipVertically self =
  wrapNewGObject mkPixbuf $
  (\(Pixbuf arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gdk_pixbuf_flip argPtr1 arg2)
{-# LINE 688 "./Graphics/UI/Gtk/Gdk/Pixbuf.chs" #-}
    self
    (fromBool False)

-- | Rotates a pixbuf by a multiple of 90 degrees, and returns the result in a
-- new pixbuf.
--
pixbufRotateSimple :: Pixbuf -> PixbufRotation -> IO Pixbuf
pixbufRotateSimple self angle =
  wrapNewGObject mkPixbuf $
  (\(Pixbuf arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gdk_pixbuf_rotate_simple argPtr1 arg2)
{-# LINE 698 "./Graphics/UI/Gtk/Gdk/Pixbuf.chs" #-}
    self
    ((fromIntegral . fromEnum) angle)

-- | The possible rotations which can be passed to 'pixbufRotateSimple'.
--
-- To make them easier to use, their numerical values are the actual degrees.
--
data PixbufRotation = PixbufRotateNone
                    | PixbufRotateCounterclockwise
                    | PixbufRotateUpsidedown
                    | PixbufRotateClockwise
                    
instance Enum PixbufRotation where
  fromEnum PixbufRotateNone = 0
  fromEnum PixbufRotateCounterclockwise = 90
  fromEnum PixbufRotateUpsidedown = 180
  fromEnum PixbufRotateClockwise = 270

  toEnum 0 = PixbufRotateNone
  toEnum 90 = PixbufRotateCounterclockwise
  toEnum 180 = PixbufRotateUpsidedown
  toEnum 270 = PixbufRotateClockwise
  toEnum unmatched = error ("PixbufRotation.toEnum: Cannot match " ++ show unmatched)

  succ PixbufRotateNone = PixbufRotateCounterclockwise
  succ PixbufRotateCounterclockwise = PixbufRotateUpsidedown
  succ PixbufRotateUpsidedown = PixbufRotateClockwise
  succ _ = undefined

  pred PixbufRotateCounterclockwise = PixbufRotateNone
  pred PixbufRotateUpsidedown = PixbufRotateCounterclockwise
  pred PixbufRotateClockwise = PixbufRotateUpsidedown
  pred _ = undefined

  enumFromTo x y | fromEnum x == fromEnum y = [ y ]
                 | otherwise = x : enumFromTo (succ x) y
  enumFrom x = enumFromTo x PixbufRotateClockwise
  enumFromThen _ _ =     error "Enum PixbufRotation: enumFromThen not implemented"
  enumFromThenTo _ _ _ =     error "Enum PixbufRotation: enumFromThenTo not implemented"

{-# LINE 706 "./Graphics/UI/Gtk/Gdk/Pixbuf.chs" #-}


-- | Add an opacity layer to the 'Pixbuf'.
--
-- * This function returns a copy of the given @src@
-- 'Pixbuf', leaving @src@ unmodified.
-- The new 'Pixbuf' has an alpha (opacity)
-- channel which defaults to @255@ (fully opaque pixels)
-- unless @src@ already had an alpha channel in which case
-- the original values are kept.
-- Passing in a color triple @(r,g,b)@ makes all
-- pixels that have this color fully transparent
-- (opacity of @0@). The pixel color itself remains unchanged
-- during this substitution.
--
pixbufAddAlpha :: Pixbuf -> Maybe (Word8, Word8, Word8) -> IO Pixbuf
pixbufAddAlpha pb Nothing = wrapNewGObject mkPixbuf $
  (\(Pixbuf arg1) arg2 arg3 arg4 arg5 -> withForeignPtr arg1 $ \argPtr1 ->gdk_pixbuf_add_alpha argPtr1 arg2 arg3 arg4 arg5) pb (fromBool False) 0 0 0
pixbufAddAlpha pb (Just (r,g,b)) = wrapNewGObject mkPixbuf $
  (\(Pixbuf arg1) arg2 arg3 arg4 arg5 -> withForeignPtr arg1 $ \argPtr1 ->gdk_pixbuf_add_alpha argPtr1 arg2 arg3 arg4 arg5) pb (fromBool True)
    (fromIntegral r) (fromIntegral g) (fromIntegral b)

-- | Copy a rectangular portion into another 'Pixbuf'.
--
-- The source 'Pixbuf' remains unchanged. Converion between
-- different formats is done automatically.
--
pixbufCopyArea ::
    Pixbuf -- ^ Source pixbuf
 -> Int -- ^ Source X coordinate within the source pixbuf
 -> Int -- ^ Source Y coordinate within the source pixbuf
 -> Int -- ^ Width of the area to copy
 -> Int -- ^ Height of the area to copy
 -> Pixbuf -- ^ Destination pixbuf
 -> Int -- ^ X coordinate within the destination pixbuf
 -> Int -- ^ Y coordinate within the destination pixbuf
 -> IO ()
pixbufCopyArea src srcX srcY srcWidth srcHeight dest destX destY =
  (\(Pixbuf arg1) arg2 arg3 arg4 arg5 (Pixbuf arg6) arg7 arg8 -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg6 $ \argPtr6 ->gdk_pixbuf_copy_area argPtr1 arg2 arg3 arg4 arg5 argPtr6 arg7 arg8) src
    (fromIntegral srcX) (fromIntegral srcY)
    (fromIntegral srcWidth) (fromIntegral srcHeight)
    dest (fromIntegral destX) (fromIntegral destY)

-- | Fills a 'Pixbuf' with a color.
--
-- * The passed-in color is a quadruple consisting of the red, green, blue
-- and alpha component of the pixel. If the 'Pixbuf' does not
-- have an alpha channel, the alpha value is ignored.
--
pixbufFill :: Pixbuf -> Word8 -> Word8 -> Word8 -> Word8 -> IO ()
pixbufFill pb red green blue alpha = (\(Pixbuf arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gdk_pixbuf_fill argPtr1 arg2) pb
  ((fromIntegral red) `shiftL` 24 .|.
   (fromIntegral green) `shiftL` 16 .|.
   (fromIntegral blue) `shiftL` 8 .|.
   (fromIntegral alpha))


-- | Take a screenshot of a 'Drawable'.
--
-- * This function creates a 'Pixbuf' and fills it with the image
-- currently in the 'Drawable' (which might be invalid if the
-- window is obscured or minimized). Note that this transfers data from
-- the server to the client on X Windows.
--
-- * This function will return a 'Pixbuf' with no alpha channel
-- containing the part of the 'Drawable' specified by the
-- rectangle. The function will return @Nothing@ if the window
-- is not currently visible.
--
-- Removed in Gtk3.
pixbufGetFromDrawable :: DrawableClass d => d -> Rectangle -> IO (Maybe Pixbuf)
pixbufGetFromDrawable d (Rectangle x y width height) =
  maybeNull (wrapNewGObject mkPixbuf) $
  (\(Pixbuf arg1) (Drawable arg2) (Colormap arg3) arg4 arg5 arg6 arg7 arg8 arg9 -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->withForeignPtr arg3 $ \argPtr3 ->gdk_pixbuf_get_from_drawable argPtr1 argPtr2 argPtr3 arg4 arg5 arg6 arg7 arg8 arg9)
{-# LINE 780 "./Graphics/UI/Gtk/Gdk/Pixbuf.chs" #-}
    (Pixbuf nullForeignPtr) (toDrawable d) (Colormap nullForeignPtr)
    (fromIntegral x) (fromIntegral y) 0 0
    (fromIntegral width) (fromIntegral height)


-- | Takes the opacity values in a rectangular portion of a pixbuf and
-- thresholds them to produce a bi-level alpha mask that can be used
-- as a clipping mask for a drawable.
--
-- Removed in Gtk3.
pixbufRenderThresholdAlpha ::
    Pixbuf -- ^ A pixbuf.
 -> Bitmap -- ^ Bitmap where the bilevel mask will be painted to.
 -> Int -- ^ Source X coordinate.
 -> Int -- ^ source Y coordinate.
 -> Int -- ^ Destination X coordinate.
 -> Int -- ^ Destination Y coordinate.
 -> Int -- ^ Width of region to threshold, or -1 to use pixbuf width
 -> Int -- ^ Height of region to threshold, or -1 to use pixbuf height
 -> Int -- ^ Opacity values below this will be painted as zero; all other values will be painted as one.
 -> IO ()
pixbufRenderThresholdAlpha src dest srcX srcY destX destY w h at =
  withForeignPtr (unPixmap dest) $ \destPtr ->
  (\(Pixbuf arg1) arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 -> withForeignPtr arg1 $ \argPtr1 ->gdk_pixbuf_render_threshold_alpha argPtr1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9) src
                                                (castPtr destPtr)
                                                (fromIntegral srcX)
                                                (fromIntegral srcY)
                                                (fromIntegral destX)
                                                (fromIntegral destY)
                                                (fromIntegral w)
                                                (fromIntegral h)
                                                (fromIntegral at)





-- | Creates a pixmap and a mask bitmap which are returned and renders
-- a pixbuf and its corresponding thresholded alpha mask to them. This
-- is merely a convenience function; applications that need to render
-- pixbufs with dither offsets or to given drawables should use
-- 'Graphics.UI.Gtk.Gdk.Drawable.drawPixbuf', and
-- 'pixbufRenderThresholdAlpha'.
--
-- The pixmap that is created uses the 'Colormap' specified by
-- colormap. This colormap must match the colormap of the window where
-- the pixmap will eventually be used or an error will result.
--
-- If the pixbuf does not have an alpha channel, then the returned
-- mask will be @Nothing@.
--
-- Removed in Gtk3.
pixbufRenderPixmapAndMaskForColormap ::
    Pixbuf -- ^ A pixbuf.
 -> Colormap -- ^ A Colormap
 -> Int -- ^ Threshold value for opacity values
 -> IO (Pixmap, Maybe Bitmap) -- ^ (Created pixmap, created mask)
pixbufRenderPixmapAndMaskForColormap pixbuf colormap threshold =
  alloca $ \pmRetPtr ->
    alloca $ \bmRetPtr -> do
      (\(Pixbuf arg1) (Colormap arg2) arg3 arg4 arg5 -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gdk_pixbuf_render_pixmap_and_mask_for_colormap argPtr1 argPtr2 arg3 arg4 arg5) pixbuf
                                                                 colormap
                                                                 (castPtr pmRetPtr) -- seems to reject Pixmap**, so cast
                                                                 (castPtr bmRetPtr)
                                                                 (fromIntegral threshold)
      pm <- wrapNewGObject mkPixmap (peek pmRetPtr :: IO (Ptr Pixmap))
      bm <- maybeNull (wrapNewGObject mkPixmap) (peek bmRetPtr :: IO (Ptr Bitmap))
      return (pm, bm)

foreign import ccall unsafe "gdk_pixbuf_get_colorspace"
  gdk_pixbuf_get_colorspace :: ((Ptr Pixbuf) -> (IO CInt))

foreign import ccall unsafe "gdk_pixbuf_get_n_channels"
  gdk_pixbuf_get_n_channels :: ((Ptr Pixbuf) -> (IO CInt))

foreign import ccall unsafe "gdk_pixbuf_get_has_alpha"
  gdk_pixbuf_get_has_alpha :: ((Ptr Pixbuf) -> (IO CInt))

foreign import ccall unsafe "gdk_pixbuf_get_bits_per_sample"
  gdk_pixbuf_get_bits_per_sample :: ((Ptr Pixbuf) -> (IO CInt))

foreign import ccall unsafe "gdk_pixbuf_get_pixels"
  gdk_pixbuf_get_pixels :: ((Ptr Pixbuf) -> (IO (Ptr CUChar)))

foreign import ccall unsafe "gdk_pixbuf_get_width"
  gdk_pixbuf_get_width :: ((Ptr Pixbuf) -> (IO CInt))

foreign import ccall unsafe "gdk_pixbuf_get_height"
  gdk_pixbuf_get_height :: ((Ptr Pixbuf) -> (IO CInt))

foreign import ccall unsafe "gdk_pixbuf_get_rowstride"
  gdk_pixbuf_get_rowstride :: ((Ptr Pixbuf) -> (IO CInt))

foreign import ccall unsafe "gdk_pixbuf_get_option"
  gdk_pixbuf_get_option :: ((Ptr Pixbuf) -> ((Ptr CChar) -> (IO (Ptr CChar))))

foreign import ccall unsafe "gdk_pixbuf_error_quark"
  gdk_pixbuf_error_quark :: CUInt

foreign import ccall unsafe "gdk_pixbuf_new_from_file"
  gdk_pixbuf_new_from_file :: ((Ptr CChar) -> ((Ptr (Ptr ())) -> (IO (Ptr Pixbuf))))

foreign import ccall safe "gdk_pixbuf_new_from_file_at_size"
  gdk_pixbuf_new_from_file_at_size :: ((Ptr CChar) -> (CInt -> (CInt -> ((Ptr (Ptr ())) -> (IO (Ptr Pixbuf))))))

foreign import ccall safe "gdk_pixbuf_new_from_file_at_scale"
  gdk_pixbuf_new_from_file_at_scale :: ((Ptr CChar) -> (CInt -> (CInt -> (CInt -> ((Ptr (Ptr ())) -> (IO (Ptr Pixbuf)))))))

foreign import ccall unsafe "gdk_pixbuf_savev"
  gdk_pixbuf_savev :: ((Ptr Pixbuf) -> ((Ptr CChar) -> ((Ptr CChar) -> ((Ptr (Ptr CChar)) -> ((Ptr (Ptr CChar)) -> ((Ptr (Ptr ())) -> (IO CInt)))))))

foreign import ccall safe "gdk_pixbuf_new"
  gdk_pixbuf_new :: (CInt -> (CInt -> (CInt -> (CInt -> (CInt -> (IO (Ptr Pixbuf)))))))

foreign import ccall safe "gdk_pixbuf_new_from_data"
  gdk_pixbuf_new_from_data :: ((Ptr CUChar) -> (CInt -> (CInt -> (CInt -> (CInt -> (CInt -> (CInt -> ((FunPtr ((Ptr CUChar) -> ((Ptr ()) -> (IO ())))) -> ((Ptr ()) -> (IO (Ptr Pixbuf)))))))))))

foreign import ccall safe "gdk_pixbuf_new_from_xpm_data"
  gdk_pixbuf_new_from_xpm_data :: ((Ptr (Ptr CChar)) -> (IO (Ptr Pixbuf)))

foreign import ccall unsafe "gdk_pixbuf_new_from_inline"
  gdk_pixbuf_new_from_inline :: (CInt -> ((Ptr CUChar) -> (CInt -> ((Ptr (Ptr ())) -> (IO (Ptr Pixbuf))))))

foreign import ccall unsafe "gdk_pixbuf_new_subpixbuf"
  gdk_pixbuf_new_subpixbuf :: ((Ptr Pixbuf) -> (CInt -> (CInt -> (CInt -> (CInt -> (IO (Ptr Pixbuf)))))))

foreign import ccall unsafe "gdk_pixbuf_copy"
  gdk_pixbuf_copy :: ((Ptr Pixbuf) -> (IO (Ptr Pixbuf)))

foreign import ccall safe "gdk_pixbuf_scale_simple"
  gdk_pixbuf_scale_simple :: ((Ptr Pixbuf) -> (CInt -> (CInt -> (CInt -> (IO (Ptr Pixbuf))))))

foreign import ccall unsafe "gdk_pixbuf_scale"
  gdk_pixbuf_scale :: ((Ptr Pixbuf) -> ((Ptr Pixbuf) -> (CInt -> (CInt -> (CInt -> (CInt -> (CDouble -> (CDouble -> (CDouble -> (CDouble -> (CInt -> (IO ()))))))))))))

foreign import ccall unsafe "gdk_pixbuf_composite"
  gdk_pixbuf_composite :: ((Ptr Pixbuf) -> ((Ptr Pixbuf) -> (CInt -> (CInt -> (CInt -> (CInt -> (CDouble -> (CDouble -> (CDouble -> (CDouble -> (CInt -> (CInt -> (IO ())))))))))))))

foreign import ccall safe "gdk_pixbuf_flip"
  gdk_pixbuf_flip :: ((Ptr Pixbuf) -> (CInt -> (IO (Ptr Pixbuf))))

foreign import ccall safe "gdk_pixbuf_rotate_simple"
  gdk_pixbuf_rotate_simple :: ((Ptr Pixbuf) -> (CInt -> (IO (Ptr Pixbuf))))

foreign import ccall unsafe "gdk_pixbuf_add_alpha"
  gdk_pixbuf_add_alpha :: ((Ptr Pixbuf) -> (CInt -> (CUChar -> (CUChar -> (CUChar -> (IO (Ptr Pixbuf)))))))

foreign import ccall unsafe "gdk_pixbuf_copy_area"
  gdk_pixbuf_copy_area :: ((Ptr Pixbuf) -> (CInt -> (CInt -> (CInt -> (CInt -> ((Ptr Pixbuf) -> (CInt -> (CInt -> (IO ())))))))))

foreign import ccall unsafe "gdk_pixbuf_fill"
  gdk_pixbuf_fill :: ((Ptr Pixbuf) -> (CUInt -> (IO ())))

foreign import ccall unsafe "gdk_pixbuf_get_from_drawable"
  gdk_pixbuf_get_from_drawable :: ((Ptr Pixbuf) -> ((Ptr Drawable) -> ((Ptr Colormap) -> (CInt -> (CInt -> (CInt -> (CInt -> (CInt -> (CInt -> (IO (Ptr Pixbuf)))))))))))

foreign import ccall unsafe "gdk_pixbuf_render_threshold_alpha"
  gdk_pixbuf_render_threshold_alpha :: ((Ptr Pixbuf) -> ((Ptr ()) -> (CInt -> (CInt -> (CInt -> (CInt -> (CInt -> (CInt -> (CInt -> (IO ()))))))))))

foreign import ccall unsafe "gdk_pixbuf_render_pixmap_and_mask_for_colormap"
  gdk_pixbuf_render_pixmap_and_mask_for_colormap :: ((Ptr Pixbuf) -> ((Ptr Colormap) -> ((Ptr Pixmap) -> ((Ptr (Ptr ())) -> (CInt -> (IO ()))))))
