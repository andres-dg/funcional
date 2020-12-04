
{-# LINE 2 "./Graphics/UI/Gtk/Selectors/FontSelection.chs" #-}
-- -*-haskell-*-
-- GIMP Toolkit (GTK) Widget FontSelection
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
-- A widget for selecting fonts
--
module Graphics.UI.Gtk.Selectors.FontSelection (
-- * Detail
--
-- | The 'FontSelection' widget lists the available fonts, styles and sizes,
-- allowing the user to select a font. It is used in the 'FontSelectionDialog'
-- widget to provide a dialog box for selecting fonts.
--
-- To set the font which is initially selected, use
-- 'fontSelectionSetFontName'.
--
-- To get the selected font use 'fontSelectionGetFontName'.
--
-- To change the text which is shown in the preview area, use
-- 'fontSelectionSetPreviewText'.

-- * Class Hierarchy
-- |
-- @
-- | 'GObject'
-- | +----'Object'
-- | +----'Widget'
-- | +----'Container'
-- | +----'Box'
-- | +----'VBox'
-- | +----FontSelection
-- @

-- * Types
  FontSelection,
  FontSelectionClass,
  castToFontSelection, gTypeFontSelection,
  toFontSelection,

-- * Constructors
  fontSelectionNew,

-- * Methods
  fontSelectionGetFontName,
  fontSelectionSetFontName,
  fontSelectionGetPreviewText,
  fontSelectionSetPreviewText,

-- * Attributes
  fontSelectionFontName,
  fontSelectionPreviewText,
  ) where

import Control.Monad (liftM)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.Attributes
import System.Glib.Properties
import Graphics.UI.Gtk.Abstract.Object (makeNewObject)
import Graphics.UI.Gtk.Types
{-# LINE 83 "./Graphics/UI/Gtk/Selectors/FontSelection.chs" #-}


{-# LINE 85 "./Graphics/UI/Gtk/Selectors/FontSelection.chs" #-}

--------------------
-- Constructors

-- | Creates a new 'FontSelection'.
--
fontSelectionNew :: IO FontSelection
fontSelectionNew =
  makeNewObject mkFontSelection $
  liftM (castPtr :: Ptr Widget -> Ptr FontSelection) $
  gtk_font_selection_new
{-# LINE 96 "./Graphics/UI/Gtk/Selectors/FontSelection.chs" #-}

--------------------
-- Methods

-- | Gets the currently-selected font name.
--
fontSelectionGetFontName :: (FontSelectionClass self, GlibString string) => self
 -> IO (Maybe string) -- ^ returns the name of the currently selected font, or
                      -- @Nothing@ if no font is selected.
fontSelectionGetFontName self =
  (\(FontSelection arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_font_selection_get_font_name argPtr1)
{-# LINE 107 "./Graphics/UI/Gtk/Selectors/FontSelection.chs" #-}
    (toFontSelection self)
  >>= maybePeek readUTFString

-- | Sets the currently-selected font.
--
fontSelectionSetFontName :: (FontSelectionClass self, GlibString string) => self
 -> string -- ^ @fontname@ - a fontname.
 -> IO Bool -- ^ returns @True@ if the font was found.
fontSelectionSetFontName self fontname =
  liftM toBool $
  withUTFString fontname $ \fontnamePtr ->
  (\(FontSelection arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_font_selection_set_font_name argPtr1 arg2)
{-# LINE 119 "./Graphics/UI/Gtk/Selectors/FontSelection.chs" #-}
    (toFontSelection self)
    fontnamePtr

-- | Gets the text displayed in the preview area.
--
fontSelectionGetPreviewText :: (FontSelectionClass self, GlibString string) => self -> IO string
fontSelectionGetPreviewText self =
  (\(FontSelection arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_font_selection_get_preview_text argPtr1)
{-# LINE 127 "./Graphics/UI/Gtk/Selectors/FontSelection.chs" #-}
    (toFontSelection self)
  >>= peekUTFString

-- | Sets the text displayed in the preview area.
--
fontSelectionSetPreviewText :: (FontSelectionClass self, GlibString string) => self -> string -> IO ()
fontSelectionSetPreviewText self text =
  withUTFString text $ \textPtr ->
  (\(FontSelection arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_font_selection_set_preview_text argPtr1 arg2)
{-# LINE 136 "./Graphics/UI/Gtk/Selectors/FontSelection.chs" #-}
    (toFontSelection self)
    textPtr

--------------------
-- Attributes

-- | The X string that represents this font.
--
-- Default value: \"\"
--
fontSelectionFontName :: (FontSelectionClass self, GlibString string) => Attr self string
fontSelectionFontName = newAttrFromStringProperty "font_name"

-- | The text to display in order to demonstrate the selected font.
--
-- Default value: \"abcdefghijk ABCDEFGHIJK\"
--
fontSelectionPreviewText :: (FontSelectionClass self, GlibString string) => Attr self string
fontSelectionPreviewText = newAttr
  fontSelectionGetPreviewText
  fontSelectionSetPreviewText

foreign import ccall unsafe "gtk_font_selection_new"
  gtk_font_selection_new :: (IO (Ptr Widget))

foreign import ccall unsafe "gtk_font_selection_get_font_name"
  gtk_font_selection_get_font_name :: ((Ptr FontSelection) -> (IO (Ptr CChar)))

foreign import ccall safe "gtk_font_selection_set_font_name"
  gtk_font_selection_set_font_name :: ((Ptr FontSelection) -> ((Ptr CChar) -> (IO CInt)))

foreign import ccall unsafe "gtk_font_selection_get_preview_text"
  gtk_font_selection_get_preview_text :: ((Ptr FontSelection) -> (IO (Ptr CChar)))

foreign import ccall safe "gtk_font_selection_set_preview_text"
  gtk_font_selection_set_preview_text :: ((Ptr FontSelection) -> ((Ptr CChar) -> (IO ())))
