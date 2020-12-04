
{-# LINE 2 "./Graphics/UI/Gtk/Selectors/ColorButton.chs" #-}
-- -*-haskell-*-
-- GIMP Toolkit (GTK) Widget ColorButton
--
-- Author : Duncan Coutts
--
-- Created: 5 April 2005
--
-- Copyright (C) 2005 Duncan Coutts
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
-- A button to launch a color selection dialog
--
-- * Module available since Gtk+ version 2.4
--
module Graphics.UI.Gtk.Selectors.ColorButton (
-- * Detail
--
-- | The 'ColorButton' is a button which displays the currently selected color
-- an allows to open a color selection dialog to change the color. It is
-- suitable widget for selecting a color in a preference dialog.

-- * Class Hierarchy
-- |
-- @
-- | 'GObject'
-- | +----'Object'
-- | +----'Widget'
-- | +----'Container'
-- | +----'Bin'
-- | +----'Button'
-- | +----ColorButton
-- @


-- * Types
  ColorButton,
  ColorButtonClass,
  castToColorButton, gTypeColorButton,
  toColorButton,

-- * Constructors
  colorButtonNew,
  colorButtonNewWithColor,

-- * Methods
  colorButtonSetColor,
  colorButtonGetColor,
  colorButtonSetAlpha,
  colorButtonGetAlpha,
  colorButtonSetUseAlpha,
  colorButtonGetUseAlpha,
  colorButtonSetTitle,
  colorButtonGetTitle,

-- * Attributes
  colorButtonUseAlpha,
  colorButtonTitle,
  colorButtonAlpha,

-- * Signals
  onColorSet,
  afterColorSet,

  ) where

import Control.Monad (liftM)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.Attributes
import Graphics.UI.Gtk.Abstract.Object (makeNewObject)
import Graphics.UI.Gtk.Types
{-# LINE 88 "./Graphics/UI/Gtk/Selectors/ColorButton.chs" #-}
import Graphics.UI.Gtk.Signals
{-# LINE 89 "./Graphics/UI/Gtk/Selectors/ColorButton.chs" #-}
import Graphics.UI.Gtk.General.Structs (Color)


{-# LINE 92 "./Graphics/UI/Gtk/Selectors/ColorButton.chs" #-}


--------------------
-- Constructors

-- | Creates a new color button. This returns a widget in the form of a small
-- button containing a swatch representing the current selected color. When the
-- button is clicked, a color-selection dialog will open, allowing the user to
-- select a color. The swatch will be updated to reflect the new color when the
-- user finishes.
--
colorButtonNew :: IO ColorButton
colorButtonNew =
  makeNewObject mkColorButton $
  liftM (castPtr :: Ptr Widget -> Ptr ColorButton) $
  gtk_color_button_new
{-# LINE 108 "./Graphics/UI/Gtk/Selectors/ColorButton.chs" #-}

-- | Creates a new color button.
--
colorButtonNewWithColor ::
    Color -- ^ @color@ - A 'Color' to set the current color with.
 -> IO ColorButton
colorButtonNewWithColor color =
  makeNewObject mkColorButton $
  liftM (castPtr :: Ptr Widget -> Ptr ColorButton) $
  with color $ \colorPtr ->
  gtk_color_button_new_with_color
{-# LINE 119 "./Graphics/UI/Gtk/Selectors/ColorButton.chs" #-}
    (castPtr colorPtr)

--------------------
-- Methods

-- | Sets the current color to be @color@.
--
colorButtonSetColor :: ColorButtonClass self => self
 -> Color -- ^ @color@ - A 'Color' to set the current color with.
 -> IO ()
colorButtonSetColor self color =
  with color $ \colorPtr ->
  (\(ColorButton arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_color_button_set_color argPtr1 arg2)
{-# LINE 132 "./Graphics/UI/Gtk/Selectors/ColorButton.chs" #-}
    (toColorButton self)
    (castPtr colorPtr)

-- | Returns the current color value.
--
colorButtonGetColor :: ColorButtonClass self => self -> IO Color
colorButtonGetColor self =
  alloca $ \colorPtr ->
  (\(ColorButton arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_color_button_get_color argPtr1 arg2)
{-# LINE 141 "./Graphics/UI/Gtk/Selectors/ColorButton.chs" #-}
    (toColorButton self)
    (castPtr colorPtr)
  >> peek colorPtr >>= \color ->
  return color

-- | Sets the current opacity to be @alpha@.
--
colorButtonSetAlpha :: ColorButtonClass self => self
 -> Word16 -- ^ @alpha@ - an integer between 0 and 65535.
 -> IO ()
colorButtonSetAlpha self alpha =
  (\(ColorButton arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_color_button_set_alpha argPtr1 arg2)
{-# LINE 153 "./Graphics/UI/Gtk/Selectors/ColorButton.chs" #-}
    (toColorButton self)
    (fromIntegral alpha)

-- | Returns the current alpha value.
--
colorButtonGetAlpha :: ColorButtonClass self => self
 -> IO Word16 -- ^ returns an integer between 0 and 65535.
colorButtonGetAlpha self =
  liftM fromIntegral $
  (\(ColorButton arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_color_button_get_alpha argPtr1)
{-# LINE 163 "./Graphics/UI/Gtk/Selectors/ColorButton.chs" #-}
    (toColorButton self)

-- | Sets whether or not the color button should use the alpha channel.
--
colorButtonSetUseAlpha :: ColorButtonClass self => self
 -> Bool -- ^ @useAlpha@ - @True@ if color button should use alpha channel,
          -- @False@ if not.
 -> IO ()
colorButtonSetUseAlpha self useAlpha =
  (\(ColorButton arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_color_button_set_use_alpha argPtr1 arg2)
{-# LINE 173 "./Graphics/UI/Gtk/Selectors/ColorButton.chs" #-}
    (toColorButton self)
    (fromBool useAlpha)

-- | Does the color selection dialog use the alpha channel?
--
colorButtonGetUseAlpha :: ColorButtonClass self => self
 -> IO Bool -- ^ returns @True@ if the color sample uses alpha channel,
            -- @False@ if not.
colorButtonGetUseAlpha self =
  liftM toBool $
  (\(ColorButton arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_color_button_get_use_alpha argPtr1)
{-# LINE 184 "./Graphics/UI/Gtk/Selectors/ColorButton.chs" #-}
    (toColorButton self)

-- | Sets the title for the color selection dialog.
--
colorButtonSetTitle :: (ColorButtonClass self, GlibString string) => self
 -> string -- ^ @title@ - String containing new window title.
 -> IO ()
colorButtonSetTitle self title =
  withUTFString title $ \titlePtr ->
  (\(ColorButton arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_color_button_set_title argPtr1 arg2)
{-# LINE 194 "./Graphics/UI/Gtk/Selectors/ColorButton.chs" #-}
    (toColorButton self)
    titlePtr

-- | Gets the title of the color selection dialog.
--
colorButtonGetTitle :: (ColorButtonClass self, GlibString string) => self
 -> IO string -- ^ returns An internal string, do not free the return value
colorButtonGetTitle self =
  (\(ColorButton arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_color_button_get_title argPtr1)
{-# LINE 203 "./Graphics/UI/Gtk/Selectors/ColorButton.chs" #-}
    (toColorButton self)
  >>= peekUTFString

--------------------
-- Attributes

-- | If this property is set to @True@, the color swatch on the button is
-- rendered against a checkerboard background to show its opacity and the
-- opacity slider is displayed in the color selection dialog.
--
-- Default value: @False@
--
colorButtonUseAlpha :: ColorButtonClass self => Attr self Bool
colorButtonUseAlpha = newAttr
  colorButtonGetUseAlpha
  colorButtonSetUseAlpha

-- | The title of the color selection dialog
--
-- Default value: \"Pick a Color\"
--
colorButtonTitle :: (ColorButtonClass self, GlibString string) => Attr self string
colorButtonTitle = newAttr
  colorButtonGetTitle
  colorButtonSetTitle

-- | The selected opacity value (0 fully transparent, 65535 fully opaque).
--
-- Allowed values: \<= 65535
--
-- Default value: 65535
--
colorButtonAlpha :: ColorButtonClass self => Attr self Word16
colorButtonAlpha = newAttr
  colorButtonGetAlpha
  colorButtonSetAlpha

--------------------
-- Signals

-- | The 'colorSet' signal is emitted when the user selects a color. When
-- handling this signal, use 'colorButtonGetColor' and 'colorButtonGetAlpha' to
-- find out which color was just selected.
--
onColorSet, afterColorSet :: ColorButtonClass self => self
 -> IO ()
 -> IO (ConnectId self)
onColorSet = connect_NONE__NONE "color_set" False
afterColorSet = connect_NONE__NONE "color_set" True

foreign import ccall safe "gtk_color_button_new"
  gtk_color_button_new :: (IO (Ptr Widget))

foreign import ccall safe "gtk_color_button_new_with_color"
  gtk_color_button_new_with_color :: ((Ptr ()) -> (IO (Ptr Widget)))

foreign import ccall safe "gtk_color_button_set_color"
  gtk_color_button_set_color :: ((Ptr ColorButton) -> ((Ptr ()) -> (IO ())))

foreign import ccall safe "gtk_color_button_get_color"
  gtk_color_button_get_color :: ((Ptr ColorButton) -> ((Ptr ()) -> (IO ())))

foreign import ccall safe "gtk_color_button_set_alpha"
  gtk_color_button_set_alpha :: ((Ptr ColorButton) -> (CUShort -> (IO ())))

foreign import ccall safe "gtk_color_button_get_alpha"
  gtk_color_button_get_alpha :: ((Ptr ColorButton) -> (IO CUShort))

foreign import ccall safe "gtk_color_button_set_use_alpha"
  gtk_color_button_set_use_alpha :: ((Ptr ColorButton) -> (CInt -> (IO ())))

foreign import ccall safe "gtk_color_button_get_use_alpha"
  gtk_color_button_get_use_alpha :: ((Ptr ColorButton) -> (IO CInt))

foreign import ccall safe "gtk_color_button_set_title"
  gtk_color_button_set_title :: ((Ptr ColorButton) -> ((Ptr CChar) -> (IO ())))

foreign import ccall safe "gtk_color_button_get_title"
  gtk_color_button_get_title :: ((Ptr ColorButton) -> (IO (Ptr CChar)))