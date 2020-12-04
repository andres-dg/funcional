
{-# LINE 2 "./Graphics/UI/Gtk/Printing/PaperSize.chs" #-}
-- -*-haskell-*-
-- GIMP Toolkit (GTK) Widget PaperSize
--
-- Author : Andy Stewart
--
-- Created: 28 Mar 2010
--
-- Copyright (C) 2010 Andy Stewart
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
-- |
-- Maintainer : gtk2hs-users@lists.sourceforge.net
-- Stability : provisional
-- Portability : portable (depends on GHC)
--
-- Support for named paper sizes
--
module Graphics.UI.Gtk.Printing.PaperSize (

-- * Detail
--
-- | 'PaperSize' handles paper sizes. It uses the
-- standard called \"PWG 5101.1-2002 PWG: Standard for Media Standardized
-- Names\" to name the paper sizes (and to get the data for the page sizes). In
-- addition to standard paper sizes, 'PaperSize' allows
-- to construct custom paper sizes with arbitrary dimensions.
--
-- The 'PaperSize' object stores not only the
-- dimensions (width and height) of a paper size and its name, it also provides
-- default print margins.
--
-- Printing support has been added in Gtk+ 2.10.


-- * Types
  PaperSize(..),
  mkPaperSize,

-- * Enums
  Unit(..),

-- * Constructors
  paperSizeNew,
  paperSizeNewFromPpd,
  paperSizeNewCustom,

-- * Methods
  paperSizeCopy,
  paperSizeIsEqual,
  paperSizeGetName,
  paperSizeGetDisplayName,
  paperSizeGetPpdName,
  paperSizeGetWidth,
  paperSizeGetHeight,
  paperSizeIsCustom,
  paperSizeSetSize,
  paperSizeGetDefaultTopMargin,
  paperSizeGetDefaultBottomMargin,
  paperSizeGetDefaultLeftMargin,
  paperSizeGetDefaultRightMargin,
  paperSizeGetDefault,


  paperSizeGetPaperSizes,

  ) where

import Control.Monad (liftM)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.GList


{-# LINE 85 "./Graphics/UI/Gtk/Printing/PaperSize.chs" #-}

--------------------
-- Types
newtype PaperSize = PaperSize (ForeignPtr (PaperSize))
{-# LINE 89 "./Graphics/UI/Gtk/Printing/PaperSize.chs" #-}

--------------------
-- Enums
data Unit = UnitPixel
          | UnitPoints
          | UnitInch
          | UnitMm
          deriving (Enum,Bounded,Eq,Show)

{-# LINE 93 "./Graphics/UI/Gtk/Printing/PaperSize.chs" #-}

--------------------
-- Constructors

mkPaperSize :: Ptr PaperSize -> IO PaperSize
mkPaperSize pPtr = do
  size <- newForeignPtr pPtr paper_size_free
  return (PaperSize size)

foreign import ccall unsafe "&gtk_paper_size_free"
  paper_size_free :: FinalizerPtr PaperSize


-- | Creates a new 'PaperSize' object by parsing a PWG
-- 5101.1-2002 paper name.
--
-- If @name@ is Nothing, the default paper size is returned, see 'paperSizeGetDefault'.
--
-- * Available since Gtk+ version 2.10
--
paperSizeNew :: GlibString string
 => Maybe string -- ^ @name@ - a paper size name, or 'Nothing'
 -> IO PaperSize
paperSizeNew name =
  maybeWith withUTFString name $ \namePtr ->
  gtk_paper_size_new
{-# LINE 119 "./Graphics/UI/Gtk/Printing/PaperSize.chs" #-}
    namePtr
  >>= mkPaperSize

-- | Creates a new 'PaperSize' object by using PPD
-- information.
--
-- If @ppdName@ is not a recognized PPD paper name, @ppdDisplayName@,
-- @width@ and @height@ are used to construct a custom 'PaperSize' object.
--
-- * Available since Gtk+ version 2.10
--
paperSizeNewFromPpd :: GlibString string
 => string -- ^ @ppdName@ - a PPD paper name
 -> string -- ^ @ppdDisplayName@ - the corresponding human-readable name
 -> Double -- ^ @width@ - the paper width, in points
 -> Double -- ^ @height@ - the paper height in points
 -> IO PaperSize
paperSizeNewFromPpd ppdName ppdDisplayName width height =
  withUTFString ppdDisplayName $ \ppdDisplayNamePtr ->
  withUTFString ppdName $ \ppdNamePtr ->
  gtk_paper_size_new_from_ppd
{-# LINE 140 "./Graphics/UI/Gtk/Printing/PaperSize.chs" #-}
    ppdNamePtr
    ppdDisplayNamePtr
    (realToFrac width)
    (realToFrac height)
  >>= mkPaperSize

-- | Creates a new 'PaperSize' object with the given
-- parameters.
--
-- * Available since Gtk+ version 2.10
--
paperSizeNewCustom :: GlibString string
 => string -- ^ @name@ - the paper name
 -> string -- ^ @displayName@ - the human-readable name
 -> Double -- ^ @width@ - the paper width, in units of @unit@
 -> Double -- ^ @height@ - the paper height, in units of @unit@
 -> Unit -- ^ @unit@ - the unit for @width@ and @height@
 -> IO PaperSize
paperSizeNewCustom name displayName width height unit =
  withUTFString displayName $ \displayNamePtr ->
  withUTFString name $ \namePtr ->
  gtk_paper_size_new_custom
{-# LINE 162 "./Graphics/UI/Gtk/Printing/PaperSize.chs" #-}
    namePtr
    displayNamePtr
    (realToFrac width)
    (realToFrac height)
    ((fromIntegral . fromEnum) unit)
  >>= mkPaperSize


--------------------
-- Methods

-- | Copies an existing 'PaperSize'.
--
-- * Available since Gtk+ version 2.10
--
paperSizeCopy :: PaperSize
 -> IO PaperSize -- ^ returns a copy of @other@
paperSizeCopy self =
  (\(PaperSize arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_paper_size_copy argPtr1) self >>= mkPaperSize

-- | Compares two 'PaperSize' objects.
--
-- * Available since Gtk+ version 2.10
--
paperSizeIsEqual :: PaperSize
 -> PaperSize -- ^ @size2@ - another 'PaperSize' object
 -> IO Bool -- ^ returns @True@, if @size1@ and @size2@ represent
             -- the same paper size
paperSizeIsEqual self size2 =
  liftM toBool $
  (\(PaperSize arg1) (PaperSize arg2) -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gtk_paper_size_is_equal argPtr1 argPtr2)
{-# LINE 193 "./Graphics/UI/Gtk/Printing/PaperSize.chs" #-}
    self
    size2

-- | Gets the name of the 'PaperSize'.
--
-- * Available since Gtk+ version 2.10
--
paperSizeGetName :: GlibString string => PaperSize
 -> IO string -- ^ returns the name of @size@
paperSizeGetName self =
  (\(PaperSize arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_paper_size_get_name argPtr1)
{-# LINE 204 "./Graphics/UI/Gtk/Printing/PaperSize.chs" #-}
    self
  >>= peekUTFString

-- | Gets the human-readable name of the 'PaperSize'.
--
-- * Available since Gtk+ version 2.10
--
paperSizeGetDisplayName :: GlibString string => PaperSize
 -> IO string -- ^ returns the human-readable name of @size@
paperSizeGetDisplayName self =
  (\(PaperSize arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_paper_size_get_display_name argPtr1)
{-# LINE 215 "./Graphics/UI/Gtk/Printing/PaperSize.chs" #-}
    self
  >>= peekUTFString

-- | Gets the PPD name of the 'PaperSize', which may be
--
-- * Available since Gtk+ version 2.10
--
paperSizeGetPpdName :: GlibString string => PaperSize
 -> IO (Maybe string) -- ^ returns the PPD name of @size@, or 'Nothing'
paperSizeGetPpdName self =
  (\(PaperSize arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_paper_size_get_ppd_name argPtr1)
{-# LINE 226 "./Graphics/UI/Gtk/Printing/PaperSize.chs" #-}
    self
  >>= maybePeekUTFString

-- | Gets the paper width of the 'PaperSize', in units
-- of @unit@.
--
-- * Available since Gtk+ version 2.10
--
paperSizeGetWidth :: PaperSize
 -> Unit -- ^ @unit@ - the unit for the return value
 -> IO Double -- ^ returns the paper width
paperSizeGetWidth self unit =
  liftM realToFrac $
  (\(PaperSize arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_paper_size_get_width argPtr1 arg2)
{-# LINE 240 "./Graphics/UI/Gtk/Printing/PaperSize.chs" #-}
    self
    ((fromIntegral . fromEnum) unit)

-- | Gets the paper height of the 'PaperSize', in units
-- of @unit@.
--
-- * Available since Gtk+ version 2.10
--
paperSizeGetHeight :: PaperSize
 -> Unit -- ^ @unit@ - the unit for the return value
 -> IO Double -- ^ returns the paper height
paperSizeGetHeight self unit =
  liftM realToFrac $
  (\(PaperSize arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_paper_size_get_height argPtr1 arg2)
{-# LINE 254 "./Graphics/UI/Gtk/Printing/PaperSize.chs" #-}
    self
    ((fromIntegral . fromEnum) unit)

-- | Returns @True@ if @size@ is not a standard paper size.
--
paperSizeIsCustom :: PaperSize
 -> IO Bool -- ^ returns whether @size@ is a custom paper size.
paperSizeIsCustom self =
  liftM toBool $
  (\(PaperSize arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_paper_size_is_custom argPtr1)
{-# LINE 264 "./Graphics/UI/Gtk/Printing/PaperSize.chs" #-}
    self

-- | Changes the dimensions of a @size@ to @width@ x @height@.
--
-- * Available since Gtk+ version 2.10
--
paperSizeSetSize :: PaperSize
 -> Double -- ^ @width@ - the new width in units of @unit@
 -> Double -- ^ @height@ - the new height in units of @unit@
 -> Unit -- ^ @unit@ - the unit for @width@ and @height@
 -> IO ()
paperSizeSetSize self width height unit =
  (\(PaperSize arg1) arg2 arg3 arg4 -> withForeignPtr arg1 $ \argPtr1 ->gtk_paper_size_set_size argPtr1 arg2 arg3 arg4)
{-# LINE 277 "./Graphics/UI/Gtk/Printing/PaperSize.chs" #-}
    self
    (realToFrac width)
    (realToFrac height)
    ((fromIntegral . fromEnum) unit)

-- | Gets the default top margin for the 'PaperSize'.
--
-- * Available since Gtk+ version 2.10
--
paperSizeGetDefaultTopMargin :: PaperSize
 -> Unit -- ^ @unit@ - the unit for the return value
 -> IO Double -- ^ returns the default top margin
paperSizeGetDefaultTopMargin self unit =
  liftM realToFrac $
  (\(PaperSize arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_paper_size_get_default_top_margin argPtr1 arg2)
{-# LINE 292 "./Graphics/UI/Gtk/Printing/PaperSize.chs" #-}
    self
    ((fromIntegral . fromEnum) unit)

-- | Gets the default bottom margin for the 'PaperSize'.
--
-- * Available since Gtk+ version 2.10
--
paperSizeGetDefaultBottomMargin :: PaperSize
 -> Unit -- ^ @unit@ - the unit for the return value
 -> IO Double -- ^ returns the default bottom margin
paperSizeGetDefaultBottomMargin self unit =
  liftM realToFrac $
  (\(PaperSize arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_paper_size_get_default_bottom_margin argPtr1 arg2)
{-# LINE 305 "./Graphics/UI/Gtk/Printing/PaperSize.chs" #-}
    self
    ((fromIntegral . fromEnum) unit)

-- | Gets the default left margin for the 'PaperSize'.
--
-- * Available since Gtk+ version 2.10
--
paperSizeGetDefaultLeftMargin :: PaperSize
 -> Unit -- ^ @unit@ - the unit for the return value
 -> IO Double -- ^ returns the default left margin
paperSizeGetDefaultLeftMargin self unit =
  liftM realToFrac $
  (\(PaperSize arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_paper_size_get_default_left_margin argPtr1 arg2)
{-# LINE 318 "./Graphics/UI/Gtk/Printing/PaperSize.chs" #-}
    self
    ((fromIntegral . fromEnum) unit)

-- | Gets the default right margin for the 'PaperSize'.
--
-- * Available since Gtk+ version 2.10
--
paperSizeGetDefaultRightMargin :: PaperSize
 -> Unit -- ^ @unit@ - the unit for the return value
 -> IO Double -- ^ returns the default right margin
paperSizeGetDefaultRightMargin self unit =
  liftM realToFrac $
  (\(PaperSize arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_paper_size_get_default_right_margin argPtr1 arg2)
{-# LINE 331 "./Graphics/UI/Gtk/Printing/PaperSize.chs" #-}
    self
    ((fromIntegral . fromEnum) unit)

-- | Returns the name of the default paper size, which depends on the current
-- locale.
--
-- * Available since Gtk+ version 2.10
--
paperSizeGetDefault :: GlibString string
 => IO string -- ^ returns the name of the default paper size.
paperSizeGetDefault =
  gtk_paper_size_get_default
{-# LINE 343 "./Graphics/UI/Gtk/Printing/PaperSize.chs" #-}
  >>= peekUTFString




-- | Creates a list of known paper sizes.
--
-- * Available since Gtk+ version 2.12
--
paperSizeGetPaperSizes ::
    Bool -- ^ @includeCustom@ - whether to include custom
                            -- paper sizes as defined in the page setup dialog
 -> IO [PaperSize]
paperSizeGetPaperSizes includeCustom = do
  glist <- gtk_paper_size_get_paper_sizes (fromBool includeCustom)
  list <- fromGList glist
  mapM mkPaperSize list

foreign import ccall safe "gtk_paper_size_new"
  gtk_paper_size_new :: ((Ptr CChar) -> (IO (Ptr PaperSize)))

foreign import ccall safe "gtk_paper_size_new_from_ppd"
  gtk_paper_size_new_from_ppd :: ((Ptr CChar) -> ((Ptr CChar) -> (CDouble -> (CDouble -> (IO (Ptr PaperSize))))))

foreign import ccall safe "gtk_paper_size_new_custom"
  gtk_paper_size_new_custom :: ((Ptr CChar) -> ((Ptr CChar) -> (CDouble -> (CDouble -> (CInt -> (IO (Ptr PaperSize)))))))

foreign import ccall safe "gtk_paper_size_copy"
  gtk_paper_size_copy :: ((Ptr PaperSize) -> (IO (Ptr PaperSize)))

foreign import ccall safe "gtk_paper_size_is_equal"
  gtk_paper_size_is_equal :: ((Ptr PaperSize) -> ((Ptr PaperSize) -> (IO CInt)))

foreign import ccall safe "gtk_paper_size_get_name"
  gtk_paper_size_get_name :: ((Ptr PaperSize) -> (IO (Ptr CChar)))

foreign import ccall safe "gtk_paper_size_get_display_name"
  gtk_paper_size_get_display_name :: ((Ptr PaperSize) -> (IO (Ptr CChar)))

foreign import ccall safe "gtk_paper_size_get_ppd_name"
  gtk_paper_size_get_ppd_name :: ((Ptr PaperSize) -> (IO (Ptr CChar)))

foreign import ccall safe "gtk_paper_size_get_width"
  gtk_paper_size_get_width :: ((Ptr PaperSize) -> (CInt -> (IO CDouble)))

foreign import ccall safe "gtk_paper_size_get_height"
  gtk_paper_size_get_height :: ((Ptr PaperSize) -> (CInt -> (IO CDouble)))

foreign import ccall safe "gtk_paper_size_is_custom"
  gtk_paper_size_is_custom :: ((Ptr PaperSize) -> (IO CInt))

foreign import ccall safe "gtk_paper_size_set_size"
  gtk_paper_size_set_size :: ((Ptr PaperSize) -> (CDouble -> (CDouble -> (CInt -> (IO ())))))

foreign import ccall safe "gtk_paper_size_get_default_top_margin"
  gtk_paper_size_get_default_top_margin :: ((Ptr PaperSize) -> (CInt -> (IO CDouble)))

foreign import ccall safe "gtk_paper_size_get_default_bottom_margin"
  gtk_paper_size_get_default_bottom_margin :: ((Ptr PaperSize) -> (CInt -> (IO CDouble)))

foreign import ccall safe "gtk_paper_size_get_default_left_margin"
  gtk_paper_size_get_default_left_margin :: ((Ptr PaperSize) -> (CInt -> (IO CDouble)))

foreign import ccall safe "gtk_paper_size_get_default_right_margin"
  gtk_paper_size_get_default_right_margin :: ((Ptr PaperSize) -> (CInt -> (IO CDouble)))

foreign import ccall safe "gtk_paper_size_get_default"
  gtk_paper_size_get_default :: (IO (Ptr CChar))

foreign import ccall safe "gtk_paper_size_get_paper_sizes"
  gtk_paper_size_get_paper_sizes :: (CInt -> (IO (Ptr ())))
