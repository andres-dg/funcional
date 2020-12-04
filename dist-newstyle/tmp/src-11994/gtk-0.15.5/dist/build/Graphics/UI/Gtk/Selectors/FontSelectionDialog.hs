
{-# LINE 2 "./Graphics/UI/Gtk/Selectors/FontSelectionDialog.chs" #-}
-- -*-haskell-*-
-- GIMP Toolkit (GTK) Widget FontSelectionDialog
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
-- A dialog box for selecting fonts
--
module Graphics.UI.Gtk.Selectors.FontSelectionDialog (
-- * Detail
--
-- | The 'FontSelectionDialog' widget is a dialog box for selecting a font.
--
-- To set the font which is initially selected, use
-- 'fontSelectionDialogSetFontName'.
--
-- To get the selected font use 'fontSelectionDialogGetFontName'.
--
-- To change the text which is shown in the preview area, use
-- 'fontSelectionDialogSetPreviewText'.

-- * Class Hierarchy
-- |
-- @
-- | 'GObject'
-- | +----'Object'
-- | +----'Widget'
-- | +----'Container'
-- | +----'Bin'
-- | +----'Window'
-- | +----'Dialog'
-- | +----FontSelectionDialog
-- @

-- * Types
  FontSelectionDialog,
  FontSelectionDialogClass,
  castToFontSelectionDialog, gTypeFontSelectionDialog,
  toFontSelectionDialog,

-- * Constructors
  fontSelectionDialogNew,

-- * Methods
  fontSelectionDialogGetFontName,
  fontSelectionDialogSetFontName,
  fontSelectionDialogGetPreviewText,
  fontSelectionDialogSetPreviewText,

  fontSelectionDialogGetCancelButton,
  fontSelectionDialogGetOkButton,


  fontSelectionDialogGetFontSelection,


-- * Attributes
  fontSelectionDialogPreviewText,
  ) where

import Control.Monad (liftM)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.Attributes
import Graphics.UI.Gtk.Abstract.Object (makeNewObject)
import Graphics.UI.Gtk.Types
{-# LINE 87 "./Graphics/UI/Gtk/Selectors/FontSelectionDialog.chs" #-}


{-# LINE 89 "./Graphics/UI/Gtk/Selectors/FontSelectionDialog.chs" #-}

--------------------
-- Constructors

-- | Creates a new 'FontSelectionDialog'.
--
fontSelectionDialogNew :: GlibString string
 => string -- ^ @title@ - the title of the dialog box.
 -> IO FontSelectionDialog
fontSelectionDialogNew title =
  makeNewObject mkFontSelectionDialog $
  liftM (castPtr :: Ptr Widget -> Ptr FontSelectionDialog) $
  withUTFString title $ \titlePtr ->
  gtk_font_selection_dialog_new
{-# LINE 103 "./Graphics/UI/Gtk/Selectors/FontSelectionDialog.chs" #-}
    titlePtr

--------------------
-- Methods

-- | Gets the currently-selected font name.
--
fontSelectionDialogGetFontName :: (FontSelectionDialogClass self, GlibString string) => self
 -> IO (Maybe string) -- ^ returns the currently-selected font name, or
                      -- @Nothing@ if no font is selected.
fontSelectionDialogGetFontName self =
  (\(FontSelectionDialog arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_font_selection_dialog_get_font_name argPtr1)
{-# LINE 115 "./Graphics/UI/Gtk/Selectors/FontSelectionDialog.chs" #-}
    (toFontSelectionDialog self)
  >>= maybePeek readUTFString

-- | Sets the currently-selected font.
--
fontSelectionDialogSetFontName :: (FontSelectionDialogClass self, GlibString string) => self
 -> string -- ^ @fontname@ - a fontname.
 -> IO Bool -- ^ returns @True@ if the font was found.
fontSelectionDialogSetFontName self fontname =
  liftM toBool $
  withUTFString fontname $ \fontnamePtr ->
  (\(FontSelectionDialog arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_font_selection_dialog_set_font_name argPtr1 arg2)
{-# LINE 127 "./Graphics/UI/Gtk/Selectors/FontSelectionDialog.chs" #-}
    (toFontSelectionDialog self)
    fontnamePtr

-- | Gets the text displayed in the preview area.
--
fontSelectionDialogGetPreviewText :: (FontSelectionDialogClass self, GlibString string) => self -> IO string
fontSelectionDialogGetPreviewText self =
  (\(FontSelectionDialog arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_font_selection_dialog_get_preview_text argPtr1)
{-# LINE 135 "./Graphics/UI/Gtk/Selectors/FontSelectionDialog.chs" #-}
    (toFontSelectionDialog self)
  >>= peekUTFString

-- | Sets the text displayed in the preview area.
--
fontSelectionDialogSetPreviewText :: (FontSelectionDialogClass self, GlibString string) => self -> string -> IO ()
fontSelectionDialogSetPreviewText self text =
  withUTFString text $ \textPtr ->
  (\(FontSelectionDialog arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_font_selection_dialog_set_preview_text argPtr1 arg2)
{-# LINE 144 "./Graphics/UI/Gtk/Selectors/FontSelectionDialog.chs" #-}
    (toFontSelectionDialog self)
    textPtr


-- | Gets the 'Cancel' button.
--
-- * Available since Gtk+ version 2.14
--
fontSelectionDialogGetCancelButton :: FontSelectionDialogClass self => self
                                    -> IO Widget -- ^ returns the 'Widget' used in the dialog for the 'Cancel' button.
fontSelectionDialogGetCancelButton self =
  makeNewObject mkWidget $
  (\(FontSelectionDialog arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_font_selection_dialog_get_cancel_button argPtr1)
{-# LINE 157 "./Graphics/UI/Gtk/Selectors/FontSelectionDialog.chs" #-}
     (toFontSelectionDialog self)

-- | Gets the 'OK' button.
--
-- * Available since Gtk+ version 2.14
--
fontSelectionDialogGetOkButton :: FontSelectionDialogClass self => self
                               -> IO Widget -- ^ returns the 'Widget' used in the dialog for the 'OK' button.
fontSelectionDialogGetOkButton self =
  makeNewObject mkWidget $
  (\(FontSelectionDialog arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_font_selection_dialog_get_ok_button argPtr1)
{-# LINE 168 "./Graphics/UI/Gtk/Selectors/FontSelectionDialog.chs" #-}
     (toFontSelectionDialog self)




-- | Retrieves the 'FontSelection' widget embedded in the dialog.
--
-- * Available since Gtk+ version 2.22
--
fontSelectionDialogGetFontSelection :: FontSelectionDialogClass self => self
                                    -> IO FontSelection -- ^ returns the embedded 'FontSelection'
fontSelectionDialogGetFontSelection self =
  makeNewObject mkFontSelection $
  liftM (castPtr :: Ptr Widget -> Ptr FontSelection) $
  (\(FontSelectionDialog arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_font_selection_dialog_get_font_selection argPtr1)
{-# LINE 183 "./Graphics/UI/Gtk/Selectors/FontSelectionDialog.chs" #-}
     (toFontSelectionDialog self)


--------------------
-- Attributes

-- | \'previewText\' property. See 'fontSelectionDialogGetPreviewText' and
-- 'fontSelectionDialogSetPreviewText'
--
fontSelectionDialogPreviewText :: (FontSelectionDialogClass self, GlibString string) => Attr self string
fontSelectionDialogPreviewText = newAttr
  fontSelectionDialogGetPreviewText
  fontSelectionDialogSetPreviewText

foreign import ccall unsafe "gtk_font_selection_dialog_new"
  gtk_font_selection_dialog_new :: ((Ptr CChar) -> (IO (Ptr Widget)))

foreign import ccall safe "gtk_font_selection_dialog_get_font_name"
  gtk_font_selection_dialog_get_font_name :: ((Ptr FontSelectionDialog) -> (IO (Ptr CChar)))

foreign import ccall safe "gtk_font_selection_dialog_set_font_name"
  gtk_font_selection_dialog_set_font_name :: ((Ptr FontSelectionDialog) -> ((Ptr CChar) -> (IO CInt)))

foreign import ccall unsafe "gtk_font_selection_dialog_get_preview_text"
  gtk_font_selection_dialog_get_preview_text :: ((Ptr FontSelectionDialog) -> (IO (Ptr CChar)))

foreign import ccall safe "gtk_font_selection_dialog_set_preview_text"
  gtk_font_selection_dialog_set_preview_text :: ((Ptr FontSelectionDialog) -> ((Ptr CChar) -> (IO ())))

foreign import ccall safe "gtk_font_selection_dialog_get_cancel_button"
  gtk_font_selection_dialog_get_cancel_button :: ((Ptr FontSelectionDialog) -> (IO (Ptr Widget)))

foreign import ccall safe "gtk_font_selection_dialog_get_ok_button"
  gtk_font_selection_dialog_get_ok_button :: ((Ptr FontSelectionDialog) -> (IO (Ptr Widget)))

foreign import ccall safe "gtk_font_selection_dialog_get_font_selection"
  gtk_font_selection_dialog_get_font_selection :: ((Ptr FontSelectionDialog) -> (IO (Ptr Widget)))
