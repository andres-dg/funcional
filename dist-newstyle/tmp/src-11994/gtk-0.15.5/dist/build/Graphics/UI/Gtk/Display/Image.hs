
{-# LINE 2 "./Graphics/UI/Gtk/Display/Image.chs" #-}
-- -*-haskell-*-
-- GIMP Toolkit (GTK) Widget Image
--
-- Author : Axel Simon
--
-- Created: 23 May 2001
--
-- Copyright (C) 2001-2005 Axel Simon
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
-- Figure out what other functions are useful within Haskell. Maybe we should
-- support loading Pixmaps without exposing them.
--
-- Because Haskell is not the best language to modify large images directly
-- only functions are bound that allow loading images from disc or by stock
-- names.
--
-- Another function for extracting the 'Pixbuf' is added for
-- 'CellRenderer'.
--
-- |
-- Maintainer : gtk2hs-users@lists.sourceforge.net
-- Stability : provisional
-- Portability : portable (depends on GHC)
--
-- A widget displaying an image
--
module Graphics.UI.Gtk.Display.Image (
-- * Detail
--
-- | The 'Image' widget displays an image. Various kinds of object can be
-- displayed as an image; most typically, you would load a 'Pixbuf' (\"pixel
-- buffer\") from a file, and then display that. There's a convenience function
-- to do this, 'imageNewFromFile', used as follows: If the file isn't loaded
-- successfully, the image will contain a \"broken image\" icon similar to that
-- used in many web browsers. If you want to handle errors in loading the file
-- yourself, for example by displaying an error message, then load the image
-- with 'Graphics.UI.Gtk.Gdk.Pixbuf.pixbufNewFromFile', then create the
-- 'Image' with 'imageNewFromPixbuf'.
--
-- > image <- imageNewFromFile "myfile.png"
--
-- The image file may contain an animation, if so the 'Image' will display
-- an animation ('PixbufAnimation') instead of a static image.
--
-- 'Image' is a subclass of 'Misc', which implies that you can align it
-- (center, left, right) and add padding to it, using 'Misc' methods.
--
-- 'Image' is a \"no window\" widget (has no 'DrawWindow' of its own), so by
-- default does not receive events. If you want to receive events on the image,
-- such as button clicks, place the image inside a 'EventBox', then connect to
-- the event signals on the event box.
--
-- When handling events on the event box, keep in mind that coordinates in
-- the image may be different from event box coordinates due to the alignment
-- and padding settings on the image (see 'Misc'). The simplest way to solve
-- this is to set the alignment to 0.0 (left\/top), and set the padding to
-- zero. Then the origin of the image will be the same as the origin of the
-- event box.
--
-- Sometimes an application will want to avoid depending on external data
-- files, such as image files. Gtk+ comes with a program to avoid this, called
-- gdk-pixbuf-csource. This program allows you to convert an image into a C
-- variable declaration, which can then be loaded into a 'Pixbuf' using
-- 'Graphics.UI.Gtk.Gdk.Pixbuf.pixbufNewFromInline'.

-- * Class Hierarchy
-- |
-- @
-- | 'GObject'
-- | +----'Object'
-- | +----'Widget'
-- | +----'Misc'
-- | +----Image
-- @

-- * Types
  Image,
  ImageClass,
  castToImage, gTypeImage,
  toImage,
  ImageType(..),

-- * Constructors
  imageNewFromFile,
  imageNewFromPixbuf,
  imageNewFromAnimation,
  imageNewFromStock,
  imageNew,

  imageNewFromIconName,


-- * Methods
  imageGetPixbuf,
  imageSetFromPixbuf,
  imageSetFromAnimation,
  imageSetFromFile,
  imageSetFromStock,

  imageSetFromIconName,
  imageSetPixelSize,
  imageGetPixelSize,


  imageClear,


-- * Icon Sizes
  IconSize(..),

-- * Attributes
  imagePixbuf,

  imagePixmap,
  imageMask,

  imageAnimation,
  imageImage,
  imageFile,
  imageStock,
  imageIconSize,

  imagePixelSize,


  imageIconName,

  imageStorageType,
  ) where

import Control.Monad (liftM)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.Attributes
import System.Glib.Properties
import Graphics.UI.Gtk.Abstract.Object (makeNewObject)
import Graphics.UI.Gtk.Types
{-# LINE 152 "./Graphics/UI/Gtk/Display/Image.chs" #-}
import Graphics.UI.Gtk.General.StockItems
import Graphics.UI.Gtk.General.Structs (IconSize(..))


{-# LINE 156 "./Graphics/UI/Gtk/Display/Image.chs" #-}

--------------------
-- Types

-- | Describes the image data representation used by a 'Image'. If you want to
-- get the image from the widget, you can only get the currently-stored
-- representation. e.g. if the 'imageStorageType' is 'ImagePixbuf',
-- then you can call 'imageGetPixbuf' but not 'imageGetStock'. For empty
-- images, you can request any storage type (call any of the "get" functions),
-- but they will all return @Nothing@.
--
data ImageType = ImageEmpty
               | ImagePixmap
               | ImageImage
               | ImagePixbuf
               | ImageStock
               | ImageIconSet
               | ImageAnimation
               | ImageIconName
               | ImageGicon
               deriving (Enum,Show,Eq)

{-# LINE 168 "./Graphics/UI/Gtk/Display/Image.chs" #-}

--------------------
-- Constructors

-- | Creates a new 'Image' displaying the file @filename@. If the file isn't
-- found or can't be loaded, the resulting 'Image' will display a \"broken
-- image\" icon.
--
-- If the file contains an animation, the image will contain an animation.
--
-- If you need to detect failures to load the file, use
-- 'Graphics.UI.Gtk.Gdk.Pixbuf.pixbufNewFromFile'
-- to load the file yourself, then create the 'Image' from the pixbuf. (Or for
-- animations, use
-- 'Graphics.UI.Gtk.Gdk.Pixbuf.pixbufAnimationNewFromFile').
--
-- The storage type ('imageGetStorageType') of the returned image is not
-- defined, it will be whatever is appropriate for displaying the file.
--
imageNewFromFile :: GlibFilePath fp => fp -> IO Image
imageNewFromFile filename =
  makeNewObject mkImage $
  liftM (castPtr :: Ptr Widget -> Ptr Image) $
  withUTFFilePath filename $ \filenamePtr ->



  gtk_image_new_from_file
{-# LINE 196 "./Graphics/UI/Gtk/Display/Image.chs" #-}

    filenamePtr

-- | Creates a new 'Image' displaying a 'Pixbuf'.
--
-- Note that this function just creates an 'Image' from the pixbuf. The
-- 'Image' created will not react to state changes. Should you want that, you
-- should use 'imageNewFromIconSet'.
--
imageNewFromPixbuf :: Pixbuf -> IO Image
imageNewFromPixbuf pixbuf =
  makeNewObject mkImage $
  liftM (castPtr :: Ptr Widget -> Ptr Image) $
  (\(Pixbuf arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_image_new_from_pixbuf argPtr1)
{-# LINE 210 "./Graphics/UI/Gtk/Display/Image.chs" #-}
    pixbuf


imageNewFromAnimation :: (PixbufAnimationClass animation) => animation -> IO Image
imageNewFromAnimation pba = makeNewObject mkImage $
  liftM (castPtr :: Ptr Widget -> Ptr Image) $
  (\(PixbufAnimation arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_image_new_from_animation argPtr1) (toPixbufAnimation pba)


-- | Creates a 'Image' displaying a stock icon. If the stock icon name isn't
-- known, the image will be empty.
--
imageNewFromStock ::
    StockId -- ^ @stockId@ - a stock icon name
 -> IconSize -- ^ @size@ - a stock icon size
 -> IO Image
imageNewFromStock stockId size =
  makeNewObject mkImage $
  liftM (castPtr :: Ptr Widget -> Ptr Image) $
  withUTFString stockId $ \stockIdPtr ->
  gtk_image_new_from_stock
{-# LINE 231 "./Graphics/UI/Gtk/Display/Image.chs" #-}
    stockIdPtr
    ((fromIntegral . fromEnum) size)

-- | Creates a new empty 'Image' widget.
--
imageNew :: IO Image
imageNew =
  makeNewObject mkImage $
  liftM (castPtr :: Ptr Widget -> Ptr Image) $
  gtk_image_new
{-# LINE 241 "./Graphics/UI/Gtk/Display/Image.chs" #-}


-- | Creates a 'Image' displaying an icon from the current icon theme. If the
-- icon name isn't known, a \"broken image\" icon will be displayed instead. If
-- the current icon theme is changed, the icon will be updated appropriately.
--
-- * Available since Gtk+ version 2.6
--
imageNewFromIconName :: GlibString string
 => string -- ^ @iconName@ - an icon name
 -> IconSize -- ^ @size@ - a stock icon size
 -> IO Image
imageNewFromIconName iconName size =
  makeNewObject mkImage $
  liftM (castPtr :: Ptr Widget -> Ptr Image) $
  withUTFString iconName $ \iconNamePtr ->
  gtk_image_new_from_icon_name
{-# LINE 258 "./Graphics/UI/Gtk/Display/Image.chs" #-}
    iconNamePtr
    ((fromIntegral . fromEnum) size)


--------------------
-- Methods

-- | Gets the 'Pixbuf' being displayed by the 'Image'. The storage type of the
-- image must be 'ImageEmpty' or 'ImagePixbuf' (see 'imageGetStorageType').
--
imageGetPixbuf :: Image -> IO Pixbuf
imageGetPixbuf self =
  makeNewGObject mkPixbuf $ liftM castPtr $
  throwIfNull "Image.imageGetPixbuf: The image contains no Pixbuf object." $
  (\(Image arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_image_get_pixbuf argPtr1)
{-# LINE 273 "./Graphics/UI/Gtk/Display/Image.chs" #-}
    self

-- | Overwrite the current content of the 'Image' with a new 'Pixbuf'.
--
imageSetFromPixbuf :: Image -> Pixbuf -> IO ()
imageSetFromPixbuf self pixbuf =
  (\(Image arg1) (Pixbuf arg2) -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gtk_image_set_from_pixbuf argPtr1 argPtr2)
{-# LINE 280 "./Graphics/UI/Gtk/Display/Image.chs" #-}
    self
    pixbuf


imageSetFromAnimation :: (PixbufAnimationClass animation) => Image -> animation -> IO ()
imageSetFromAnimation self pba =
  (\(Image arg1) (PixbufAnimation arg2) -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gtk_image_set_from_animation argPtr1 argPtr2)
{-# LINE 287 "./Graphics/UI/Gtk/Display/Image.chs" #-}
    self
    (toPixbufAnimation pba)

-- | See 'imageNewFromFile' for details.
--
imageSetFromFile :: GlibFilePath fp => Image -> fp -> IO ()
imageSetFromFile self filename =
  withUTFFilePath filename $ \filenamePtr ->



  (\(Image arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_image_set_from_file argPtr1 arg2)
{-# LINE 299 "./Graphics/UI/Gtk/Display/Image.chs" #-}

    self
    filenamePtr

-- | See 'imageNewFromStock' for details.
--
imageSetFromStock :: Image
 -> StockId -- ^ @stockId@ - a stock icon name
 -> IconSize -- ^ @size@ - a stock icon size
 -> IO ()
imageSetFromStock self stockId size =
  withUTFString stockId $ \stockIdPtr ->
  (\(Image arg1) arg2 arg3 -> withForeignPtr arg1 $ \argPtr1 ->gtk_image_set_from_stock argPtr1 arg2 arg3)
{-# LINE 312 "./Graphics/UI/Gtk/Display/Image.chs" #-}
    self
    stockIdPtr
    ((fromIntegral . fromEnum) size)


-- | See 'imageNewFromIconName' for details.
--
-- * Available since Gtk+ version 2.6
--
imageSetFromIconName :: GlibString string => Image
 -> string -- ^ @iconName@ - an icon name
 -> IconSize -- ^ @size@ - an icon size
 -> IO ()
imageSetFromIconName self iconName size =
  withUTFString iconName $ \iconNamePtr ->
  (\(Image arg1) arg2 arg3 -> withForeignPtr arg1 $ \argPtr1 ->gtk_image_set_from_icon_name argPtr1 arg2 arg3)
{-# LINE 328 "./Graphics/UI/Gtk/Display/Image.chs" #-}
    self
    iconNamePtr
    ((fromIntegral . fromEnum) size)

-- | Sets the pixel size to use for named icons. If the pixel size is set to a
-- @value \/= -1@, it is used instead of the icon size set by
-- 'imageSetFromIconName'.
--
-- * Available since Gtk+ version 2.6
--
imageSetPixelSize :: Image
 -> Int -- ^ @pixelSize@ - the new pixel size
 -> IO ()
imageSetPixelSize self pixelSize =
  (\(Image arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_image_set_pixel_size argPtr1 arg2)
{-# LINE 343 "./Graphics/UI/Gtk/Display/Image.chs" #-}
    self
    (fromIntegral pixelSize)

-- | Gets the pixel size used for named icons.
--
-- * Available since Gtk+ version 2.6
--
imageGetPixelSize :: Image -> IO Int
imageGetPixelSize self =
  liftM fromIntegral $
  (\(Image arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_image_get_pixel_size argPtr1)
{-# LINE 354 "./Graphics/UI/Gtk/Display/Image.chs" #-}
    self



-- | Resets the image to be empty.
--
-- * Available since Gtk+ version 2.8
--
imageClear :: Image -> IO ()
imageClear self =
  (\(Image arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_image_clear argPtr1)
{-# LINE 365 "./Graphics/UI/Gtk/Display/Image.chs" #-}
    self


--------------------
-- Attributes

-- | A 'Pixbuf' to display.
--
imagePixbuf :: PixbufClass pixbuf => ReadWriteAttr Image Pixbuf pixbuf
imagePixbuf = newAttrFromObjectProperty "pixbuf"
  gdk_pixbuf_get_type
{-# LINE 376 "./Graphics/UI/Gtk/Display/Image.chs" #-}


imageAnimation :: (PixbufClass pixbuf, PixbufAnimationClass animation) => ReadWriteAttr Image animation pixbuf
imageAnimation = newAttrFromObjectProperty "pixbuf-animation"
  gdk_pixbuf_get_type
{-# LINE 381 "./Graphics/UI/Gtk/Display/Image.chs" #-}


-- | A 'Pixmap' to display.
--
imagePixmap :: PixmapClass pixmap => ReadWriteAttr Image Pixmap pixmap
imagePixmap = newAttrFromObjectProperty "pixmap"
  gdk_pixmap_get_type
{-# LINE 388 "./Graphics/UI/Gtk/Display/Image.chs" #-}

-- | Mask bitmap to use with 'Image' or 'Pixmap'.
--
imageMask :: PixmapClass pixmap => ReadWriteAttr Image Pixmap pixmap
imageMask = newAttrFromObjectProperty "mask"
  gdk_pixmap_get_type
{-# LINE 394 "./Graphics/UI/Gtk/Display/Image.chs" #-}


-- | A 'Image' to display.
--
imageImage :: ImageClass image => ReadWriteAttr Image Image image
imageImage = newAttrFromObjectProperty "image"
  gtk_image_get_type
{-# LINE 401 "./Graphics/UI/Gtk/Display/Image.chs" #-}

-- | Filename to load and display.
--
-- Default value: \"\"
--
imageFile :: GlibString string => Attr Image string
imageFile = newAttrFromStringProperty "file"

-- | Stock ID for a stock image to display.
--
-- Default value: \"\"
--
imageStock :: GlibString string => Attr Image string
imageStock = newAttrFromStringProperty "stock"

-- | Symbolic size to use for stock icon, icon set or named icon.
--
-- Allowed values: >= 0
--
-- Default value: 4
--
imageIconSize :: Attr Image Int
imageIconSize = newAttrFromIntProperty "icon-size"


-- | The pixel-size property can be used to specify a fixed size overriding
-- the icon-size property for images of type 'ImageIconName'.
--
-- Allowed values: >= -1
--
-- Default value: -1
--
imagePixelSize :: Attr Image Int
imagePixelSize = newAttr
  imageGetPixelSize
  imageSetPixelSize



-- | The name of the icon in the icon theme. If the icon theme is changed, the
-- image will be updated automatically.
--
-- Default value: \"\"
--
imageIconName :: GlibString string => Attr Image string
imageIconName = newAttrFromStringProperty "icon-name"


-- | The representation being used for image data.
--
-- Default value: 'ImageEmpty'
--
imageStorageType :: ReadAttr Image ImageType
imageStorageType = readAttrFromEnumProperty "storage-type"
  gtk_image_type_get_type
{-# LINE 456 "./Graphics/UI/Gtk/Display/Image.chs" #-}

foreign import ccall unsafe "gtk_image_new_from_file"
  gtk_image_new_from_file :: ((Ptr CChar) -> (IO (Ptr Widget)))

foreign import ccall unsafe "gtk_image_new_from_pixbuf"
  gtk_image_new_from_pixbuf :: ((Ptr Pixbuf) -> (IO (Ptr Widget)))

foreign import ccall unsafe "gtk_image_new_from_animation"
  gtk_image_new_from_animation :: ((Ptr PixbufAnimation) -> (IO (Ptr Widget)))

foreign import ccall unsafe "gtk_image_new_from_stock"
  gtk_image_new_from_stock :: ((Ptr CChar) -> (CInt -> (IO (Ptr Widget))))

foreign import ccall safe "gtk_image_new"
  gtk_image_new :: (IO (Ptr Widget))

foreign import ccall safe "gtk_image_new_from_icon_name"
  gtk_image_new_from_icon_name :: ((Ptr CChar) -> (CInt -> (IO (Ptr Widget))))

foreign import ccall unsafe "gtk_image_get_pixbuf"
  gtk_image_get_pixbuf :: ((Ptr Image) -> (IO (Ptr Pixbuf)))

foreign import ccall unsafe "gtk_image_set_from_pixbuf"
  gtk_image_set_from_pixbuf :: ((Ptr Image) -> ((Ptr Pixbuf) -> (IO ())))

foreign import ccall unsafe "gtk_image_set_from_animation"
  gtk_image_set_from_animation :: ((Ptr Image) -> ((Ptr PixbufAnimation) -> (IO ())))

foreign import ccall safe "gtk_image_set_from_file"
  gtk_image_set_from_file :: ((Ptr Image) -> ((Ptr CChar) -> (IO ())))

foreign import ccall safe "gtk_image_set_from_stock"
  gtk_image_set_from_stock :: ((Ptr Image) -> ((Ptr CChar) -> (CInt -> (IO ()))))

foreign import ccall safe "gtk_image_set_from_icon_name"
  gtk_image_set_from_icon_name :: ((Ptr Image) -> ((Ptr CChar) -> (CInt -> (IO ()))))

foreign import ccall safe "gtk_image_set_pixel_size"
  gtk_image_set_pixel_size :: ((Ptr Image) -> (CInt -> (IO ())))

foreign import ccall safe "gtk_image_get_pixel_size"
  gtk_image_get_pixel_size :: ((Ptr Image) -> (IO CInt))

foreign import ccall safe "gtk_image_clear"
  gtk_image_clear :: ((Ptr Image) -> (IO ()))

foreign import ccall unsafe "gdk_pixbuf_get_type"
  gdk_pixbuf_get_type :: CULong

foreign import ccall unsafe "gdk_pixmap_get_type"
  gdk_pixmap_get_type :: CULong

foreign import ccall unsafe "gtk_image_get_type"
  gtk_image_get_type :: CULong

foreign import ccall unsafe "gtk_image_type_get_type"
  gtk_image_type_get_type :: CULong
