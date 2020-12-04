
{-# LINE 2 "./Graphics/UI/Gtk/Selectors/ColorSelection.chs" #-}
-- -*-haskell-*-
-- GIMP Toolkit (GTK) Widget ColorSelection
--
-- Author : Duncan Coutts
--
-- Created: 2 August 2004
--
-- Copyright (C) 2004-2005 Duncan Coutts
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
-- A widget used to select a color
--
module Graphics.UI.Gtk.Selectors.ColorSelection (
-- * Detail
--
-- | The 'ColorSelection' is a widget that is used to select a color. It
-- consists of a color wheel and number of sliders and entry boxes for color
-- parameters such as hue, saturation, value, red, green, blue, and opacity. It
-- is found on the standard color selection dialog box 'ColorSelectionDialog'.

-- * Class Hierarchy
-- |
-- @
-- | 'GObject'
-- | +----'Object'
-- | +----'Widget'
-- | +----'Container'
-- | +----'Box'
-- | +----'VBox'
-- | +----ColorSelection
-- @

-- * Types
  ColorSelection,
  ColorSelectionClass,
  castToColorSelection, gTypeColorSelection,
  toColorSelection,

-- * Constructors
  colorSelectionNew,

-- * Methods
  colorSelectionGetCurrentAlpha,
  colorSelectionSetCurrentAlpha,
  colorSelectionGetCurrentColor,
  colorSelectionSetCurrentColor,
  colorSelectionGetHasOpacityControl,
  colorSelectionSetHasOpacityControl,
  colorSelectionGetHasPalette,
  colorSelectionSetHasPalette,
  colorSelectionGetPreviousAlpha,
  colorSelectionSetPreviousAlpha,
  colorSelectionGetPreviousColor,
  colorSelectionSetPreviousColor,
  colorSelectionIsAdjusting,

-- * Attributes
  colorSelectionHasOpacityControl,
  colorSelectionHasPalette,
  colorSelectionCurrentAlpha,
  colorSelectionPreviousAlpha,
  ) where

import Control.Monad (liftM)

import System.Glib.FFI
import System.Glib.Attributes
import Graphics.UI.Gtk.Abstract.Object (makeNewObject)
import Graphics.UI.Gtk.Types
{-# LINE 85 "./Graphics/UI/Gtk/Selectors/ColorSelection.chs" #-}
import Graphics.UI.Gtk.General.Structs (Color)


{-# LINE 88 "./Graphics/UI/Gtk/Selectors/ColorSelection.chs" #-}

--------------------
-- Constructors

-- | Creates a new 'ColorSelection'.
--
colorSelectionNew :: IO ColorSelection
colorSelectionNew =
  makeNewObject mkColorSelection $
  liftM (castPtr :: Ptr Widget -> Ptr ColorSelection) $
  gtk_color_selection_new
{-# LINE 99 "./Graphics/UI/Gtk/Selectors/ColorSelection.chs" #-}

--------------------
-- Methods

-- | Returns the current alpha value.
--
colorSelectionGetCurrentAlpha :: ColorSelectionClass self => self
 -> IO Int -- ^ returns an integer between 0 and 65535.
colorSelectionGetCurrentAlpha self =
  liftM fromIntegral $
  (\(ColorSelection arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_color_selection_get_current_alpha argPtr1)
{-# LINE 110 "./Graphics/UI/Gtk/Selectors/ColorSelection.chs" #-}
    (toColorSelection self)

-- | Sets the current opacity to be @alpha@. The first time this is called, it
-- will also set the original opacity to be @alpha@ too.
--
colorSelectionSetCurrentAlpha :: ColorSelectionClass self => self
 -> Int -- ^ @alpha@ - an integer between 0 and 65535.
 -> IO ()
colorSelectionSetCurrentAlpha self alpha =
  (\(ColorSelection arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_color_selection_set_current_alpha argPtr1 arg2)
{-# LINE 120 "./Graphics/UI/Gtk/Selectors/ColorSelection.chs" #-}
    (toColorSelection self)
    (fromIntegral alpha)

-- | Gets the current color in the 'ColorSelection' widget.
--
colorSelectionGetCurrentColor :: ColorSelectionClass self => self -> IO Color
colorSelectionGetCurrentColor self =
  alloca $ \colorPtr -> do
  (\(ColorSelection arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_color_selection_get_current_color argPtr1 arg2)
{-# LINE 129 "./Graphics/UI/Gtk/Selectors/ColorSelection.chs" #-}
    (toColorSelection self)
    (castPtr colorPtr)
  peek colorPtr

-- | Sets the current color to be @color@. The first time this is called, it
-- will also set the original color to be @color@ too.
--
colorSelectionSetCurrentColor :: ColorSelectionClass self => self
 -> Color -- ^ @color@ - A 'Color' to set the current color with.
 -> IO ()
colorSelectionSetCurrentColor self color =
  with color $ \colorPtr ->
  (\(ColorSelection arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_color_selection_set_current_color argPtr1 arg2)
{-# LINE 142 "./Graphics/UI/Gtk/Selectors/ColorSelection.chs" #-}
    (toColorSelection self)
    (castPtr colorPtr)

-- | Determines whether the 'ColorSelection' widget has an opacity control.
--
colorSelectionGetHasOpacityControl :: ColorSelectionClass self => self
 -> IO Bool -- ^ returns @True@ if the color selector has an opacity control.
            -- @False@ if it does't.
colorSelectionGetHasOpacityControl self =
  liftM toBool $
  (\(ColorSelection arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_color_selection_get_has_opacity_control argPtr1)
{-# LINE 153 "./Graphics/UI/Gtk/Selectors/ColorSelection.chs" #-}
    (toColorSelection self)

-- | Sets the 'ColorSelection' widget to use or not use opacity.
--
colorSelectionSetHasOpacityControl :: ColorSelectionClass self => self
 -> Bool -- ^ @hasOpacity@ - @True@ if color selector can set the opacity,
          -- @False@ otherwise.
 -> IO ()
colorSelectionSetHasOpacityControl self hasOpacity =
  (\(ColorSelection arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_color_selection_set_has_opacity_control argPtr1 arg2)
{-# LINE 163 "./Graphics/UI/Gtk/Selectors/ColorSelection.chs" #-}
    (toColorSelection self)
    (fromBool hasOpacity)

-- | Determines whether the color selector has a color palette.
--
colorSelectionGetHasPalette :: ColorSelectionClass self => self
 -> IO Bool -- ^ returns @True@ if the selector has a palette. @False@ if it
            -- hasn't.
colorSelectionGetHasPalette self =
  liftM toBool $
  (\(ColorSelection arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_color_selection_get_has_palette argPtr1)
{-# LINE 174 "./Graphics/UI/Gtk/Selectors/ColorSelection.chs" #-}
    (toColorSelection self)

-- | Sets whether to show or hide the palette.
--
colorSelectionSetHasPalette :: ColorSelectionClass self => self
 -> Bool -- ^ @hasPalette@ - @True@ if palette is to be visible, @False@
          -- otherwise.
 -> IO ()
colorSelectionSetHasPalette self hasPalette =
  (\(ColorSelection arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_color_selection_set_has_palette argPtr1 arg2)
{-# LINE 184 "./Graphics/UI/Gtk/Selectors/ColorSelection.chs" #-}
    (toColorSelection self)
    (fromBool hasPalette)

-- | Returns the previous alpha value.
--
colorSelectionGetPreviousAlpha :: ColorSelectionClass self => self
 -> IO Int -- ^ returns an integer between 0 and 65535.
colorSelectionGetPreviousAlpha self =
  liftM fromIntegral $
  (\(ColorSelection arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_color_selection_get_previous_alpha argPtr1)
{-# LINE 194 "./Graphics/UI/Gtk/Selectors/ColorSelection.chs" #-}
    (toColorSelection self)

-- | Sets the \'previous\' alpha to be @alpha@. This function should be called
-- with some hesitations, as it might seem confusing to have that alpha change.
--
colorSelectionSetPreviousAlpha :: ColorSelectionClass self => self
 -> Int -- ^ @alpha@ - an integer between 0 and 65535.
 -> IO ()
colorSelectionSetPreviousAlpha self alpha =
  (\(ColorSelection arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_color_selection_set_previous_alpha argPtr1 arg2)
{-# LINE 204 "./Graphics/UI/Gtk/Selectors/ColorSelection.chs" #-}
    (toColorSelection self)
    (fromIntegral alpha)

-- | Returns the original color value.
--
colorSelectionGetPreviousColor :: ColorSelectionClass self => self -> IO Color
colorSelectionGetPreviousColor self =
  alloca $ \colorPtr -> do
  (\(ColorSelection arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_color_selection_get_previous_color argPtr1 arg2)
{-# LINE 213 "./Graphics/UI/Gtk/Selectors/ColorSelection.chs" #-}
    (toColorSelection self)
    (castPtr colorPtr)
  peek colorPtr

-- | Sets the \'previous\' color to be @color@. This function should be called
-- with some hesitations, as it might seem confusing to have that color change.
-- Calling 'colorSelectionSetCurrentColor' will also set this color the first
-- time it is called.
--
colorSelectionSetPreviousColor :: ColorSelectionClass self => self
 -> Color -> IO ()
colorSelectionSetPreviousColor self color =
  with color $ \colorPtr ->
  (\(ColorSelection arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_color_selection_set_previous_color argPtr1 arg2)
{-# LINE 227 "./Graphics/UI/Gtk/Selectors/ColorSelection.chs" #-}
    (toColorSelection self)
    (castPtr colorPtr)

-- | Gets the current state of the widget. Returns True if the user is currently
-- dragging a color around, and False if the selection has stopped.
--
colorSelectionIsAdjusting :: ColorSelectionClass self => self -> IO Bool
colorSelectionIsAdjusting self =
  liftM toBool $
  (\(ColorSelection arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_color_selection_is_adjusting argPtr1)
{-# LINE 237 "./Graphics/UI/Gtk/Selectors/ColorSelection.chs" #-}
    (toColorSelection self)

--------------------
-- Attributes

-- | Whether the color selector should allow setting opacity.
--
-- Default value: @False@
--
colorSelectionHasOpacityControl :: ColorSelectionClass self => Attr self Bool
colorSelectionHasOpacityControl = newAttr
  colorSelectionGetHasOpacityControl
  colorSelectionSetHasOpacityControl

-- | Whether a palette should be used.
--
-- Default value: @False@
--
colorSelectionHasPalette :: ColorSelectionClass self => Attr self Bool
colorSelectionHasPalette = newAttr
  colorSelectionGetHasPalette
  colorSelectionSetHasPalette

-- | The current opacity value (0 fully transparent, 65535 fully opaque).
--
-- Allowed values: \<= 65535
--
-- Default value: 65535
--
colorSelectionCurrentAlpha :: ColorSelectionClass self => Attr self Int
colorSelectionCurrentAlpha = newAttr
  colorSelectionGetCurrentAlpha
  colorSelectionSetCurrentAlpha

-- | \'previousAlpha\' property. See 'colorSelectionGetPreviousAlpha' and
-- 'colorSelectionSetPreviousAlpha'
--
colorSelectionPreviousAlpha :: ColorSelectionClass self => Attr self Int
colorSelectionPreviousAlpha = newAttr
  colorSelectionGetPreviousAlpha
  colorSelectionSetPreviousAlpha

foreign import ccall unsafe "gtk_color_selection_new"
  gtk_color_selection_new :: (IO (Ptr Widget))

foreign import ccall unsafe "gtk_color_selection_get_current_alpha"
  gtk_color_selection_get_current_alpha :: ((Ptr ColorSelection) -> (IO CUShort))

foreign import ccall safe "gtk_color_selection_set_current_alpha"
  gtk_color_selection_set_current_alpha :: ((Ptr ColorSelection) -> (CUShort -> (IO ())))

foreign import ccall unsafe "gtk_color_selection_get_current_color"
  gtk_color_selection_get_current_color :: ((Ptr ColorSelection) -> ((Ptr ()) -> (IO ())))

foreign import ccall safe "gtk_color_selection_set_current_color"
  gtk_color_selection_set_current_color :: ((Ptr ColorSelection) -> ((Ptr ()) -> (IO ())))

foreign import ccall unsafe "gtk_color_selection_get_has_opacity_control"
  gtk_color_selection_get_has_opacity_control :: ((Ptr ColorSelection) -> (IO CInt))

foreign import ccall safe "gtk_color_selection_set_has_opacity_control"
  gtk_color_selection_set_has_opacity_control :: ((Ptr ColorSelection) -> (CInt -> (IO ())))

foreign import ccall unsafe "gtk_color_selection_get_has_palette"
  gtk_color_selection_get_has_palette :: ((Ptr ColorSelection) -> (IO CInt))

foreign import ccall safe "gtk_color_selection_set_has_palette"
  gtk_color_selection_set_has_palette :: ((Ptr ColorSelection) -> (CInt -> (IO ())))

foreign import ccall unsafe "gtk_color_selection_get_previous_alpha"
  gtk_color_selection_get_previous_alpha :: ((Ptr ColorSelection) -> (IO CUShort))

foreign import ccall safe "gtk_color_selection_set_previous_alpha"
  gtk_color_selection_set_previous_alpha :: ((Ptr ColorSelection) -> (CUShort -> (IO ())))

foreign import ccall unsafe "gtk_color_selection_get_previous_color"
  gtk_color_selection_get_previous_color :: ((Ptr ColorSelection) -> ((Ptr ()) -> (IO ())))

foreign import ccall safe "gtk_color_selection_set_previous_color"
  gtk_color_selection_set_previous_color :: ((Ptr ColorSelection) -> ((Ptr ()) -> (IO ())))

foreign import ccall unsafe "gtk_color_selection_is_adjusting"
  gtk_color_selection_is_adjusting :: ((Ptr ColorSelection) -> (IO CInt))
