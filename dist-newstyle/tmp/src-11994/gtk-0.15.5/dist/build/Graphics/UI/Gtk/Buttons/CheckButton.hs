
{-# LINE 2 "./Graphics/UI/Gtk/Buttons/CheckButton.chs" #-}
-- -*-haskell-*-
-- GIMP Toolkit (GTK) Widget CheckButton
--
-- Author : Axel Simon
--
-- Created: 15 May 2001
--
-- Copyright (C) 1999-2005 Axel Simon
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
-- Create widgets with a discrete toggle button
--
module Graphics.UI.Gtk.Buttons.CheckButton (
-- * Detail
--
-- | A 'CheckButton' places a discrete 'ToggleButton' next to a widget,
-- (usually a 'Label'). See the section on 'ToggleButton' widgets for more
-- information about toggle\/check buttons.
--
-- The important signal (\'toggled\') is also inherited from 'ToggleButton'.

-- * Class Hierarchy
-- |
-- @
-- | 'GObject'
-- | +----'Object'
-- | +----'Widget'
-- | +----'Container'
-- | +----'Bin'
-- | +----'Button'
-- | +----'ToggleButton'
-- | +----CheckButton
-- | +----'RadioButton'
-- @

-- * Types
  CheckButton,
  CheckButtonClass,
  castToCheckButton, gTypeCheckButton,
  toCheckButton,

-- * Constructors
  checkButtonNew,
  checkButtonNewWithLabel,
  checkButtonNewWithMnemonic,
  ) where

import Control.Monad (liftM)

import System.Glib.FFI
import System.Glib.UTFString
import Graphics.UI.Gtk.Abstract.Object (makeNewObject)
import Graphics.UI.Gtk.Types
{-# LINE 69 "./Graphics/UI/Gtk/Buttons/CheckButton.chs" #-}


{-# LINE 71 "./Graphics/UI/Gtk/Buttons/CheckButton.chs" #-}

--------------------
-- Constructors

-- | Creates a new 'CheckButton'.
--
checkButtonNew :: IO CheckButton
checkButtonNew =
  makeNewObject mkCheckButton $
  liftM (castPtr :: Ptr Widget -> Ptr CheckButton) $
  gtk_check_button_new
{-# LINE 82 "./Graphics/UI/Gtk/Buttons/CheckButton.chs" #-}

-- | Creates a new 'CheckButton' with a 'Label' to the right of it.
--
checkButtonNewWithLabel :: GlibString string
 => string -- ^ @label@ - the text for the check button.
 -> IO CheckButton
checkButtonNewWithLabel label =
  makeNewObject mkCheckButton $
  liftM (castPtr :: Ptr Widget -> Ptr CheckButton) $
  withUTFString label $ \labelPtr ->
  gtk_check_button_new_with_label
{-# LINE 93 "./Graphics/UI/Gtk/Buttons/CheckButton.chs" #-}
    labelPtr

-- | Creates a new 'CheckButton' containing a label. The label will be created
-- using 'Graphics.UI.Gtk.Display.Label.labelNewWithMnemonic', so underscores
-- in @label@ indicate the mnemonic for the check button.
--
checkButtonNewWithMnemonic :: GlibString string
 => string -- ^ @label@ - The text of the button, with an underscore
                   -- in front of the mnemonic character
 -> IO CheckButton
checkButtonNewWithMnemonic label =
  makeNewObject mkCheckButton $
  liftM (castPtr :: Ptr Widget -> Ptr CheckButton) $
  withUTFString label $ \labelPtr ->
  gtk_check_button_new_with_mnemonic
{-# LINE 108 "./Graphics/UI/Gtk/Buttons/CheckButton.chs" #-}
    labelPtr

foreign import ccall unsafe "gtk_check_button_new"
  gtk_check_button_new :: (IO (Ptr Widget))

foreign import ccall unsafe "gtk_check_button_new_with_label"
  gtk_check_button_new_with_label :: ((Ptr CChar) -> (IO (Ptr Widget)))

foreign import ccall unsafe "gtk_check_button_new_with_mnemonic"
  gtk_check_button_new_with_mnemonic :: ((Ptr CChar) -> (IO (Ptr Widget)))
