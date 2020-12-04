
{-# LINE 2 "./Graphics/UI/Gtk/Layout/VBox.chs" #-}
-- -*-haskell-*-
-- GIMP Toolkit (GTK) Widget VBox
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
-- A vertical container box
--
module Graphics.UI.Gtk.Layout.VBox (
-- * Detail
--
-- | 'VBox' is a container that organizes child widgets into a single column.
--
-- Use the 'Box' packing interface to determine the arrangement, spacing,
-- height, and alignment of 'VBox' children.
--
-- All children are allocated the same width.

-- * Class Hierarchy
-- |
-- @
-- | 'GObject'
-- | +----'Object'
-- | +----'Widget'
-- | +----'Container'
-- | +----'Box'
-- | +----VBox
-- | +----'ColorSelection'
-- | +----'FileChooserWidget'
-- | +----'FontSelection'
-- @

-- * Types
  VBox,
  VBoxClass,
  castToVBox, gTypeVBox,
  toVBox,

-- * Constructors
  vBoxNew,
  ) where

import Control.Monad (liftM)

import System.Glib.FFI
import Graphics.UI.Gtk.Abstract.Object (makeNewObject)
import Graphics.UI.Gtk.Types
{-# LINE 67 "./Graphics/UI/Gtk/Layout/VBox.chs" #-}


{-# LINE 69 "./Graphics/UI/Gtk/Layout/VBox.chs" #-}

--------------------
-- Constructors

-- | Creates a new 'VBox'.
--
vBoxNew ::
    Bool -- ^ @homogeneous@ - @True@ if all children are to be given equal
            -- space allotments.
 -> Int -- ^ @spacing@ - the number of pixels to place by default between
            -- children.
 -> IO VBox
vBoxNew homogeneous spacing =
  makeNewObject mkVBox $
  liftM (castPtr :: Ptr Widget -> Ptr VBox) $
  gtk_vbox_new
{-# LINE 85 "./Graphics/UI/Gtk/Layout/VBox.chs" #-}
    (fromBool homogeneous)
    (fromIntegral spacing)

foreign import ccall unsafe "gtk_vbox_new"
  gtk_vbox_new :: (CInt -> (CInt -> (IO (Ptr Widget))))
