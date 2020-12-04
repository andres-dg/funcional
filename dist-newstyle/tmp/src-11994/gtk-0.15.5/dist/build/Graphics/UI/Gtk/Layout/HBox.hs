
{-# LINE 2 "./Graphics/UI/Gtk/Layout/HBox.chs" #-}
-- -*-haskell-*-
-- GIMP Toolkit (GTK) Widget HBox
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
-- A horizontal container box
--
module Graphics.UI.Gtk.Layout.HBox (
-- * Detail
--
-- | 'HBox' is a container that organizes child widgets into a single row.
--
-- Use the 'Box' packing interface to determine the arrangement, spacing,
-- width, and alignment of 'HBox' children.
--
-- All children are allocated the same height.

-- * Class Hierarchy
-- |
-- @
-- | 'GObject'
-- | +----'Object'
-- | +----'Widget'
-- | +----'Container'
-- | +----'Box'
-- | +----HBox
-- | +----'Combo'
-- | +----'FileChooserButton'
-- | +----'Statusbar'
-- @

-- * Types
  HBox,
  HBoxClass,
  castToHBox, gTypeHBox,
  toHBox,

-- * Constructors
  hBoxNew,
  ) where

import Control.Monad (liftM)

import System.Glib.FFI
import Graphics.UI.Gtk.Abstract.Object (makeNewObject)
import Graphics.UI.Gtk.Types
{-# LINE 67 "./Graphics/UI/Gtk/Layout/HBox.chs" #-}


{-# LINE 69 "./Graphics/UI/Gtk/Layout/HBox.chs" #-}

--------------------
-- Constructors

-- | Creates a new 'HBox'.
--
hBoxNew ::
    Bool -- ^ @homogeneous@ - @True@ if all children are to be given equal
            -- space allotments.
 -> Int -- ^ @spacing@ - the number of pixels to place by default between
            -- children.
 -> IO HBox
hBoxNew homogeneous spacing =
  makeNewObject mkHBox $
  liftM (castPtr :: Ptr Widget -> Ptr HBox) $
  gtk_hbox_new
{-# LINE 85 "./Graphics/UI/Gtk/Layout/HBox.chs" #-}
    (fromBool homogeneous)
    (fromIntegral spacing)

foreign import ccall unsafe "gtk_hbox_new"
  gtk_hbox_new :: (CInt -> (CInt -> (IO (Ptr Widget))))
