
{-# LINE 2 "./Graphics/UI/Gtk/Buttons/VolumeButton.chs" #-}
-- -*-haskell-*-
-- GIMP Toolkit (GTK) Widget VolumeButton
--
-- Author : Andy Stewart
--
-- Created: 22 Mar 2010
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
-- A button which pops up a volume control
--
-- * Module available since Gtk+ version 2.12
--
module Graphics.UI.Gtk.Buttons.VolumeButton (

-- * Detail
--
-- | 'VolumeButton' is a subclass of 'ScaleButton' that has been tailored for
-- use as a volume control widget with suitable icons, tooltips and accessible
-- labels.

-- * Class Hierarchy
--
-- |
-- @
-- | 'GObject'
-- | +----'Object'
-- | +----'Widget'
-- | +----'Container'
-- | +----'Bin'
-- | +----'Button'
-- | +----'ScaleButton'
-- | +----VolumeButton
-- @


-- * Types
  VolumeButton,
  VolumeButtonClass,
  castToVolumeButton,
  toVolumeButton,

-- * Constructors
  volumeButtonNew,

  ) where

import Control.Monad (liftM)

import System.Glib.FFI
import Graphics.UI.Gtk.Abstract.Object (makeNewObject)
import Graphics.UI.Gtk.Types
{-# LINE 69 "./Graphics/UI/Gtk/Buttons/VolumeButton.chs" #-}


{-# LINE 71 "./Graphics/UI/Gtk/Buttons/VolumeButton.chs" #-}


--------------------
-- Constructors

-- | Creates a 'VolumeButton', with a range between 0.0 and 1.0, with a
-- stepping of 0.02. Volume values can be obtained and modified using the
-- functions from 'ScaleButton'.
--
volumeButtonNew :: IO VolumeButton
volumeButtonNew =
  makeNewObject mkVolumeButton $
  liftM (castPtr :: Ptr Widget -> Ptr VolumeButton) $
  gtk_volume_button_new
{-# LINE 85 "./Graphics/UI/Gtk/Buttons/VolumeButton.chs" #-}

foreign import ccall safe "gtk_volume_button_new"
  gtk_volume_button_new :: (IO (Ptr Widget))
